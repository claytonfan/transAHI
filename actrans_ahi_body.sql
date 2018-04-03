CREATE OR REPLACE PACKAGE body actrans_ahi AS
--------------------------------------------------------------------------------
-- Component Name:                 Transition
-- Number:                         actrans_ahi.pkgb
-- Applicable Segments:            EADP LAADP
-- CAS Product Used:               Oracle Relational Database, PL/SQL
--
-- Description:
--   See Package Spec
--
-- Change History:
--
-- CL  Reason    Description of Change                          Pgmr  MM/DD/YYYY
-- --  --------- --------------------------------------------   ----  ----------
--  1  ER_873    Module creation                                CCF   03/03/2011
--  2  ER_873    DOMSUPTDEV updates                              FT   06/09/2011
--  3  PR 55714  Corrected airspace overlap validation.         CCF   06/29/2011
--               Verify exited FAV is above a fix on the route.
--  4  PR 56358  Removed applicable altitude vs FAV check.      CCF   08/01/2011
--  5  PR 56840  Update airspace overlap validation              FT   10/27/2011 
--  6  ER_1192   PRED1487 Changed calls to AHI related          JAB   08/14/2014
--               cross_val functions to reference new package.
--  7  ER_1312   SIG1638 Stepped up to key changes in loc_AHI*  JAB   11/11/2014
--------------------------------------------------------------------------------
--
------------------------------------------------------------------------
-- TYPE for transition summary statistics
------------------------------------------------------------------------
TYPE Transition_Stat_Type IS RECORD (
          Num_Data_Updated    PLS_INTEGER := 0,
          Num_Data_Added      PLS_INTEGER := 0,
          Num_Data_Deleted    PLS_INTEGER := 0,
          Num_Data_Rejected   PLS_INTEGER := 0,
          Num_Spec_Added      PLS_INTEGER := 0,
          Num_ATC_Added       PLS_INTEGER := 0 );
--
------------------------------------------------------------------------
-- TYPE for building record structure disguised as an array.
-- It is used to translate CRSLOC message IDs to TRANS message IDs ad
-- message texts.
--   1 - transition message ID translated from CRSLOC ID
--   2 - completion code for message
--   3 - 'T' if the AHI spec is to be discarded
--   4 - full or partial message text
------------------------------------------------------------------------
TYPE trans_msg_type IS VARRAY(4) OF VARCHAR2(512);
T_ID       CONSTANT PLS_INTEGER := 1;  -- transition message ID
T_CODE     CONSTANT PLS_INTEGER := 2;  -- completion code
T_DISCARD  CONSTANT PLS_INTEGER := 3;  -- 'T' if discard AHI spec
T_TEXT     CONSTANT PLS_INTEGER := 4;  -- message text
--
ahi_error  EXCEPTION;
--
----------------------------------------------------------------
-- Set ahi_trace to TRUE to debug.
-- Detailed data will be logged in error report.
----------------------------------------------------------------
ahi_trace  BOOLEAN := FALSE;
--
----------------------------------------------------------------
-- Package level variable so that this is done once, and then
-- not again for the same set of transitions.
----------------------------------------------------------------
l_first_time     PLS_INTEGER    := 0;
ahi_alt_range  aclocal_util.alt_range_type;

  TYPE ahi_object_type IS RECORD (
      rte       aces_route_fix.route_id%TYPE,
      name      aces_route_fix.fix_name%TYPE,
      fix_id    aces_fix.fix_id%TYPE,
      icao_code aces_fix.icao_code%TYPE,
      alias     aces_route_fix.fix_name%TYPE,
      seqn      aces_route_fix.seq_num%TYPE,
      key       VARCHAR2(1),
      type      loc_ahi_spec.expiration_location_type%TYPE,
      alt       ahi_alt_range%TYPE );

  TYPE ahi_object_array IS TABLE OF ahi_object_type;
   TYPE ahi_airspace_type IS RECORD (
      loc    ahi_object_type,
      name   aces_route_fix.fix_name%TYPE,
      src    VARCHAR2(1),
      alt    ahi_alt_range%TYPE );

  TYPE ahi_airspace_array IS TABLE OF ahi_airspace_type;

  TYPE ahi_spec_type IS RECORD (
     alt  ahi_alt_range%TYPE,
     ext  ahi_airspace_type,
     ent  ahi_airspace_type );

  TYPE ahi_spec_array IS TABLE OF ahi_spec_type;

 l_ahi_spec          ahi_spec_array := ahi_spec_array();

--
--------------------------------------------------------------------------------
-- Procedure  : clean_all_ahis
-- Called From: transition_ahi
-- Description:
--      clean_all_ahis clears out data in the EADP local tables prior to data
--      being inserted by the transition_ahi.
--------------------------------------------------------------------------------
PROCEDURE clean_all_ahis(
     p_Facility_Id        IN     nat_facility_data.facility_id%TYPE,
     p_Local_Version      IN     ctl_set_version.set_version%TYPE ,
     p_Route_Type         IN     loc_val_route.route_type%TYPE,
     p_Err_Report         IN OUT UTL_FILE.FILE_TYPE,
     p_Gen_Report         IN OUT UTL_FILE.FILE_TYPE,
     p_rc                 IN OUT accom.completion_code%TYPE ) IS

  routine_name  VARCHAR2(80) := 'actrans_ahi.clean_all_ahis';
  l_table_name  ctl_table.table_name%TYPE;
  l_del_string  VARCHAR2(1000);

BEGIN
  ---------------------------------------------------------------
  -- Delete records from the following tables:
  --   LOC_AHI_DATA
  --   LOC_AHI_SPEC
  ---------------------------------------------------------------
  FOR each_table IN (
    SELECT table_name, subset_name FROM ctl_table
     WHERE subset_name = 'LOC_AHI_DATA'
     ORDER BY subset_name, copy_order DESC
  ) LOOP

    l_table_name := each_table.table_name;
    if l_table_name = 'LOC_AHI_DATA' then
      l_del_string := 'delete from '||l_table_name||
          ' where version = '''||p_Local_Version||''''||
          ' and NVL(owning_facility,'' '') = NVL('''||p_Facility_Id||','' '''||
          ' and route_type = '''||p_route_type||'''';
    else
      l_del_string := 'delete from '||l_table_name||
          ' where version = '''||p_Local_Version||''''||
          ' and route_type = '''||p_route_type||'''';
    end if;
    -------------------------------------------------------------
    -- Execute delete statement.
    -------------------------------------------------------------
    BEGIN
      EXECUTE IMMEDIATE l_del_string;
    EXCEPTION
      WHEN OTHERS THEN
        accom.log_error_msg(
             put_file_handler     => p_Err_Report,
             output_line          => 'Error deleting from ' ||
                                     l_table_name || ' ' ||
                                    SQLCODE || ' ' || SQLERRM,
             logging_file_handler => p_Gen_Report,
             calling_routine      => routine_name,
             Error_Id             => 'TRANS0000',
             Error_Code           => accom.g_rc_error,
             Rc                   => p_rc);
    END;
  END LOOP; -- for each_table

EXCEPTION
  WHEN OTHERS THEN
    -------------------------------------------------------------
    -- Unexpected error, write error to error log file
    -------------------------------------------------------------
    accom.log_error_msg(
         put_file_handler     => p_Err_Report,
         output_line          => 'ORA Error ' ||
                                 SQLCODE || ' ' || SQLERRM,
         logging_file_handler => p_Gen_Report,
         calling_routine      => routine_name,
         Error_Id             => 'TRANS0000',
         Error_Code           => accom.g_rc_error,
         Rc                   => p_rc);
END clean_all_ahis;
--
-------------------------------------------------------------------------------
-- Procedure  : clean_ahi
-- Called From: transition_ahi
-- Description:
--      clean_ahi clears out data for one ahi entry in its EADP detailed
--      tables prior to data being updated by the transition_ahi().
-------------------------------------------------------------------------------
PROCEDURE clean_ahi(
     p_Facility_Id        IN     nat_facility_data.facility_id%type,
     p_Local_Version      IN     ctl_set_version.set_version%type,
     p_Route_Type         IN     loc_val_route.route_type%type,
     p_Route_id           IN     loc_val_route.route_name%TYPE,
     p_Err_Report         IN OUT UTL_FILE.FILE_TYPE,
     p_Gen_Report         IN OUT UTL_FILE.FILE_TYPE,
     p_rc                 IN OUT accom.completion_code%type) is

  routine_name  VARCHAR2(80) := 'actrans_ahi.clean_ahi';
  l_table_name  ctl_table.table_name%TYPE;
  l_del_string  VARCHAR2(1000);

BEGIN
  ------------------------------------------------------------------
  -- Delete data from all detailed data for a route in LOC_AHI_DATA.
  ------------------------------------------------------------------
  FOR each_table IN (
      SELECT table_name, subset_name
        FROM ctl_table
       WHERE subset_name =   'LOC_AHI_DATA'
       ORDER BY copy_order DESC ) LOOP

    l_table_name := each_table.table_name;
    if l_table_name = 'LOC_AHI_DATA' then
      l_del_string := 'delete from '||l_table_name||
         ' where version  = '''||p_Local_Version||''''||
         ' and NVL(owning_facility,'' '') = NVL('''||p_Facility_Id||','' '''||
         ' and route_type = '''||p_route_type||''''||
         ' and route_id   = '''||p_route_id||'''';
    else
      l_del_string := 'delete from '||l_table_name||
           ' where version  = '''||p_Local_Version||''''||
           ' and route_type = '''||p_route_type||''''||
           ' and route_id   = '''||p_route_id||'''';
    end if;

    BEGIN
      EXECUTE IMMEDIATE l_del_string;
    EXCEPTION
        WHEN OTHERS THEN
          accom.log_error_msg(
            put_file_handler     => p_Err_Report,
            output_line          => 'Error deleting from ' ||
                                    l_table_name || ' for ' ||
                                    p_route_type || ' ' ||
                                    p_route_id   || ' ' ||
                                    SQLCODE || ' ' || SQLERRM,
            logging_file_handler => p_Gen_Report,
            calling_routine      => routine_name,
            Error_Id             => 'TRANS0000',
            Error_Code           => accom.g_rc_error,
            Rc                   => p_rc);
    END;       -- execute delete statement
  END LOOP;    -- for each table
EXCEPTION
  WHEN OTHERS THEN
    -------------------------------------------------------------
    -- Unexpected error, write error to error log file
    -------------------------------------------------------------
    accom.log_error_msg(
         put_file_handler     => p_Err_Report,
         output_line          => 'ORA Error ' ||
                                 SQLCODE || ' ' || SQLERRM,
         logging_file_handler => p_Gen_Report,
         calling_routine      => routine_name,
         Error_Id             => 'TRANS0000',
         Error_Code           => accom.g_rc_error,
         Rc                   => p_rc);
END clean_ahi;
--
--------------------------------------------------------------------------------
-- Procedure  : transition_ahi_pass_1
-- Description:
--   Verify that all initiation/expiration locations are adapted.
--   If parent key char is 'F', location is expected to be a fix or ATC point.
--     If location is an adapted unique fix or an adapted ATC point.
--       If location is found in MRG_FIX (non-unique fix)
--         log error
--       Otherwise, location is an unadapted ATC point
--         create ATC point in LOC_ATC_point, set assoc level to DERIVE,
--         commit and log error.
--   If parent key character is 'A', location is expected to be an airport.
--     If location is found in MRG_AIRPORT, location is AIRPORT.
--     Otherwise, log error.
--------------------------------------------------------------------------------
PROCEDURE transition_ahi_pass_1(
        p_Facility_Id       IN     nat_facility_data.facility_id%TYPE,
        p_ACES_Version      IN     ctl_set_version.set_version%TYPE,
        p_Local_Version     IN     ctl_set_version.set_version%TYPE,
        p_ACES_route_type   IN     aces_route.route_type%TYPE,
        p_Local_route_type  IN     loc_ahi_data.route_type%TYPE,
        p_Clean_Trans_Ind   IN     ctl_transition_types.clean_trans_ind%TYPE,
        p_Err_Report        IN OUT UTL_FILE.FILE_TYPE,
        p_Gen_Report        IN OUT UTL_FILE.FILE_TYPE,
        p_Trans_Stat        IN OUT Transition_Stat_Type,
        rc_com              IN OUT accom.completion_code%TYPE ) IS

  Subprogram_Name     VARCHAR2(50)   := 'actrans_ahi.transition_ahi_pass_1';
  comment_len_atc     PLS_INTEGER    := 0;
  comment_new_atc     loc_atc_point.atc_point_comment%TYPE := '';
  l_count             pls_integer    := 0;
BEGIN
  ------------------------------------------------------------------------------
  -- Get LOC_ATC_POINT.ATC_POINT_COMMENT length for record creation.
  ------------------------------------------------------------------------------
  SELECT DISTINCT data_length INTO comment_len_atc
   FROM  user_tab_columns
   WHERE table_name   = 'LOC_ATC_POINT'
     AND column_name  = 'ATC_POINT_COMMENT';

  ------------------------------------------------------------------------------
  -- For all fix names following F-keys that are not defined in:
  --   - Merged Fix table whose fix id and icao code are defined in ACES table
  --   - Local ATC Points table
  ------------------------------------------------------------------------------
  FOR undefined_fix IN (
    SELECT fix_name
     FROM (
          SELECT DISTINCT fix_name FROM aces_rte_fix_5_key
           WHERE version    = p_ACES_Version
             AND route_type = p_ACES_route_type
          UNION
          SELECT DISTINCT fix_name FROM aces_rte_fix_6_key
           WHERE version    = p_ACES_Version
             AND route_type = p_ACES_route_type
     ) aces
     WHERE NOT EXISTS (
          SELECT M.fix_name FROM aces_fix A, mrg_fix M
           WHERE A.version   = p_ACES_Version
             AND M.version   = p_Local_Version
             AND M.fix_name  = aces.fix_name
             AND A.fix_name  = M.fix_name
             AND A.fix_id    IS NOT NULL
             AND A.icao_code IS NOT NULL
             AND M.fix_id    = A.fix_id
             AND M.icao_code = A.icao_code
          UNION
          SELECT point_id FROM  loc_atc_point
           WHERE version   = p_Local_Version
             AND point_id  = aces.fix_name )
    ORDER BY fix_name
  ) LOOP
    -------------------------------------------------------------------
    -- If fix name is defined in MRG_FIX -- meaning that fix ID and
    -- ICAO code is not defined in ACES -- we cannot create an ATC point
    -- for it, and we cannot transition 5- or 6-key data for it. 
    --------------------------------------------------------------------
    SELECT count(*) INTO l_count FROM mrg_fix
     WHERE version  = p_Local_Version
       AND fix_name = undefined_fix.fix_name;

    IF l_count > 0 THEN

      accom.log_error_msg(
           put_file_handler     => p_Err_Report,
           output_line          => 
                p_ACES_route_type ||' F-key location '||
                undefined_fix.fix_name||' is not adapted.',
           logging_file_handler => p_Gen_Report,
           calling_routine      => Subprogram_Name,
           Error_Id             => 'TRANS0736',
           Error_Code           => accom.g_rc_warning,
           Rc                   => rc_com);
    ELSE
      DECLARE
        l_lat     aces_fix.latitude%TYPE;
        l_long    aces_fix.longitude%TYPE;
        coord_geo acmerge_com.Geodetic_Coord_T%TYPE;
      BEGIN
        ------------------------------------------------------------------------
        -- Create ATC point with lat/long from ACES fix table and normalize
        ------------------------------------------------------------------------
        SELECT latitude, longitude INTO l_lat, l_long FROM  aces_fix
         WHERE version   = p_ACES_Version
           AND fix_name  = undefined_fix.fix_name;

        coord_geo.lat  := l_lat;
        coord_geo.long := l_long;

        acmerge_com.Normalize_Aces_Coord(
             Coordinate          => coord_geo,
             Report_File_Handler => p_Err_Report );

        l_lat  := coord_geo.lat;
        l_long := coord_geo.long;
        ------------------------------------------
        -- Generate comment and insert ATC point
        ------------------------------------------
        actrans_common.generate_comment(
              p_from_version  => p_ACES_version,
              p_from_source   => 'ACES',
              p_text          => ' during ' || p_ACES_route_type ||
                                 ' AHI transition',
              p_comment_len   => comment_len_atc,
              p_comment_new   => comment_new_atc,
              p_data_key      => 'ATCPOINT ' || undefined_fix.fix_name,
              p_Err_Report    => p_Err_Report,
              p_Gen_Report    => p_Gen_Report,
              subprogram_name => Subprogram_Name,
              rc_comment      => rc_com );

        INSERT INTO loc_atc_point
          ( version, point_id, latitude, longitude, atc_point_comment )
        VALUES
          ( p_Local_Version, undefined_fix.fix_name, l_lat, l_long,
            comment_new_atc );

        p_Trans_Stat.Num_ATC_Added := p_Trans_Stat.Num_ATC_Added + 1;

        ------------------------------------------------------------------------
        -- Derive required to generate ERAM alt stratification before pass 2
        ------------------------------------------------------------------------
        rc_com := acrelease.set_assoc_level
             (p_release_version => p_Local_Version,
              p_assoc_level     => 'DERIVE');

        COMMIT;

        accom.log_error_msg(
             put_file_handler     => p_Err_Report,
             output_line          =>
                  p_ACES_route_type ||' F-key location '||
                  undefined_fix.fix_name||' is determined to be not adapted.'||
                  ' ATC point created.',
             logging_file_handler => p_Gen_Report,
             calling_routine      => Subprogram_Name,
             Error_Id             => 'TRANS0735',
             Error_Code           => accom.g_rc_error,
             Rc                   => rc_com);

      EXCEPTION
        WHEN NO_DATA_FOUND THEN
          -------------------------------------------------------
          -- Unexpected error
          -------------------------------------------------------
          accom.log_error_msg(
                put_file_handler     => p_Err_Report,
                output_line          =>
                     'Transition of ' || p_ACES_route_type ||
                     ' Auto Handoff Initiation: Insert of ATC point ' ||
                     undefined_fix.fix_name ||
                     ' failed because it is not found in the ACES table.',
                logging_file_handler => p_Gen_Report,
                calling_routine      => Subprogram_Name,
                Error_Id             => 'TRANS0000',
                Error_Code           => accom.g_rc_error,
                Rc                   => rc_com);
      END;
    END IF;                 -- Fix name defined in MRG_FIX?
  END LOOP;                 -- For all undefined fixes
  --------------------------------------------------------------
  -- For all airports following A-keys that are not defined
  -- or is not an alias that translates to a defined airport
  --------------------------------------------------------------
  FOR undefined_airport IN (
    SELECT DISTINCT airport_id
     FROM  aces_rte_arr_6_key aces
     WHERE version    = p_ACES_Version
       AND route_type = p_ACES_route_type
       AND NOT EXISTS (
            SELECT airport_id
             FROM  mrg_airport
             WHERE version    = p_Local_Version
               AND airport_id = aces.airport_id
            UNION
            SELECT A.from_airport
             FROM  mrg_airport_alias A, mrg_airport B
             WHERE A.version      = p_Local_Version
               AND A.from_airport = aces.airport_id
               AND B.version      = A.version
               AND B.airport_id   = A.to_airport )
    ORDER BY airport_id
  ) LOOP

    accom.log_error_msg(
         put_file_handler     => p_Err_Report,
         output_line          =>
              p_ACES_route_type ||' A-key location '||
              undefined_airport.airport_id||' is not adapted.',
         logging_file_handler => p_Gen_Report,
         calling_routine      => Subprogram_Name,
         Error_Id             => 'TRANS0736',
         Error_Code           => accom.g_rc_warning,
         Rc                   => rc_com);

  END LOOP;                 -- For all undefined airports

EXCEPTION
  WHEN OTHERS THEN
    -------------------------------------------------------------
    -- Unexpected error, write error to error log file
    -------------------------------------------------------------
    accom.log_error_msg(
         put_file_handler     => p_Err_Report,
         output_line          => 'ORA Error ' ||
                                 SQLCODE || ' ' || SQLERRM,
         logging_file_handler => p_Gen_Report,
         calling_routine      => Subprogram_Name,
         Error_Id             => 'TRANS0000',
         Error_Code           => accom.g_rc_error,
         Rc                   => rc_com );

END transition_ahi_pass_1;
--
--------------------------------------------------------------------------------
-- Procedure  : transition_ahi_pass_2
-- Description:
--   Pass 2 is executed only if when there are no errors in pass 1.
--   In pass 2, AHI Data is transitioned.
--   For all ACES routes,
--     If ACES route has no 6-keys
--       If local AHI route exists, delete it.
--     Otherwise, ACES routes has at least one 6-key:
--       If AHI Data record already exists, update it, otherwise create it.
--       For all 6-keys
--         For all altitude range of the each 6-key:
--           Get FAV/ARTCCs (entered airspace) that encompass the alt range
--           Search backwards for all 5-key alt ranges that together compass
--           the 6-key altitude range.
--             Fill the 6-key altitude range with 5-key altitude ranges
--             starting with the closest 5-key.
--           Get FAV/ARTCCs (exited airspace) from all 5-key altitude ranges
--           that encompass the 6-key alt range.
--           For all entered and exited airspaces:
--             Calculate an AHI specifier for each overlapped altitude ranges.
--             If AHI specifier is valid,
--               Add it to the list of AHI specifiers for the route.
--       Sort AHI specifiers for the route and insert into database.
--       For all AHI specifiers
--         Validate each AHI specifier against other AHI specifiers in database.
--         Delete invalid AHI specifiers and log messages.
--       If there are no AH specifiers for the AHI route, delete it.
--
-- How Overlaps and Gaps are handled:
--   ** Expiration and Initiation Locations **
--   6-key (Expiration) Location Altitude Ranges
--     More than one altitude ranges may follow a 6-key.
--     - If there is an overlap between altitude ranges
--       o A warning is logged and
--       o None of the AHI specifiers for the route are transitioned.
--     - If there is a gap between altitude ranges
--       o Transition continues
--   5-key (Initiation) Location Altitude Ranges
--     More than one altitude ranges may follow a 5-key.
--     - If there is an overlap between altitude ranges
--       o A warning is logged and
--       o None of the AHI specifiers for the route are transitioned.
--     - If there is a gap between altitude ranges
--       o Transition continues
--         - The altitude ranges are used to cover the associate 6-key altitude
--           range. The gap is filled by searching backward (in descending
--           sequence in the route) for more altitude ranges in other 5-keys.
--   Multiple 5-key (Initiation) Location Altitude Ranges
--     More than one 5-key may be associated with one 6-key.
--     - If there is an overlap between altitude ranges of different 5-keys
--       o Transition continues
--       o The altitude range of the 5-key closest to the 6-key is used to cover
--         the 6-key altitude range. The altitude of a distal 5-key is trimmed
--         to cover the rest of the 6-key altitude range.
--     - If there is a gap between altitude ranges of different 5-keys
--       o Transition continues
--         - The altitude ranges of the 5-keys are used to cover the 6-key
--           altitude range. The gap is filled by searching backward (in
--           descending sequence in the route) for more altitude ranges in other
--           5-keys. If necessary, the additional 5-key altitude ranges are
--           trimmed to fit the gap.
--   ** Entered and Exited Airspaces **
--   When a V-key altitude range does not have an FVA/ARTCC, it is used to
--   look-up the ERAM alt stratification for an FAV/ARTCC:
--     - If there is an overlap between a V-key alt range and an ERAM alt
--       stratification record, trim airspace alt range as follows:
--       - Lower alt overlap: the lower alt of the lowest airspace is set to
--         the lower alt of the V-key
--       - Upper Alt overlap: the upper alt of the highest airspace is set to
--         the upper alt of the V-key.
--       - If there is a gap between the V-key alt range and an ERAM alt
--         stratification record, or between two ERAM alt strat records
--         - Leave the gap as is. The final airspace list will have a gap.
--
--------------------------------------------------------------------------------
PROCEDURE transition_ahi_pass_2(
        p_Facility_Id       IN     nat_facility_data.facility_id%TYPE,
        p_ACES_Version      IN     ctl_set_version.set_version%TYPE,
        p_Local_Version     IN     ctl_set_version.set_version%TYPE,
        p_ACES_route_type   IN     aces_route.route_type%TYPE,
        p_Local_route_type  IN     loc_ahi_data.route_type%TYPE,
        p_Clean_Trans_Ind   IN     ctl_transition_types.clean_trans_ind%TYPE,
        p_Err_Report        IN OUT UTL_FILE.FILE_TYPE,
        p_Gen_Report        IN OUT UTL_FILE.FILE_TYPE,
        p_Trans_Stat        IN OUT Transition_Stat_Type,
        rc_com              IN OUT accom.completion_code%TYPE ) IS

  Subprogram_Name     VARCHAR2(50) := 'actrans_ahi.transition_ahi_pass_2';
  comment_length      PLS_INTEGER                := 0;
  comment_current     loc_ahi_data.comments%TYPE := '';
  comment_new         loc_ahi_data.comments%TYPE := '';
  l_command_string    VARCHAR2(200);
  l_transition_id     loc_ahi_trans.transition_id%TYPE := 'XXXXX';
  l_count_ahi_specs   PLS_INTEGER                := 0;

  ------------------------------------------------------------------
  -- AHI data record action
  --   Added     - AHI route added to database
  --   Updated   - AHI route updated in no-clean transition
  --   Deleted   - AHI route has no 6-key in ACES in no-clean transition
  --   Discarded - ACES route has 6-key but has Local AHI record
  --   None      - ACES Route has no 6-keys and no Local AHI record
  ------------------------------------------------------------------
  ahi_none     CONSTANT VARCHAR2(10) := 'None';
  ahi_create   CONSTANT VARCHAR2(10) := 'Created';
  ahi_update   CONSTANT VARCHAR2(10) := 'Updated';
  ahi_delete   CONSTANT VARCHAR2(10) := 'Deleted';
  ahi_discard  CONSTANT VARCHAR2(10) := 'Discarded';
  ahi_data_action       VARCHAR2(10) := ahi_none;
  ahi_data_count        PLS_INTEGER  := 0;

  TYPE  cur_type IS REF CURSOR;
  r_cur cur_type;

  --

   TYPE overlap_record is RECORD (
    point_id      loc_atc_over_space.POINT_ID%type,
    point_icao    mrg_fix_over_space.ICAO_CODE%type  := null,
    lower_alt     mrg_fix_over_space.lower_alt%type := null,
    upper_alt     mrg_fix_over_space.upper_alt%type := null,
    fav_or_aor    mrg_fix_over_space.fav_or_aor%type := null,
    module_id     mrg_fix_over_space.module_id%type := null
    );
  type Overspace_Array_t is table of
                             overlap_record index by binary_integer;

  ------------------------------------------------------------------------------
  -- Function   : trace_msg
  -- Description: Write text to report for debugging.
  ------------------------------------------------------------------------------
  PROCEDURE trace_msg( p_text VARCHAR2 ) IS
    rc        accom.completion_code%TYPE;
  BEGIN
    IF ahi_trace THEN
      accom.log_error_msg(
           put_file_handler     => p_Err_Report,
           output_line          => p_text,
           logging_file_handler => p_Gen_Report,
           calling_routine      => 'trans_ahi/trace_msg',
           Error_Id             => 'TRANS****',
           Error_Code           => accom.g_rc_no_error,
           Rc                   => rc );
    END IF;
  END trace_msg;
  --
  ------------------------------------------------------------------------------
  -- Procedure  : print_one_airspace
  -- Description: Write airspace data to error report
  ------------------------------------------------------------------------------
  PROCEDURE print_one_airspace (
       p_asp1    IN     ahi_airspace_type
  ) IS
  BEGIN
    UTL_FILE.put_line( p_Err_Report, '   '||
                       to_char(p_asp1.alt.lower,'099')||' '||
                       to_char(p_asp1.alt.upper,'099')||' '||
                       rpad(p_asp1.name,5)||' '||
                       ( CASE p_asp1.src WHEN 'V' THEN 'V-key'
                                         WHEN 'S' THEN 'Strat'
                                         ELSE          'VStra' END ) ||
                       ' ' || p_asp1.loc.name );
  END print_one_airspace;
  --
  ------------------------------------------------------------------------------
  -- Procedure  : print_airspace_list
  -- Description: Write airspace list to error report
  ------------------------------------------------------------------------------
  PROCEDURE print_airspace_list (
       p_text    IN     VARCHAR2,
       p_asp     IN     ahi_airspace_array
  ) IS
    Subprogram_Name    VARCHAR2(50) := 'actrans_ahi.print_airspace_list';
    rc                 accom.completion_code%TYPE;
 BEGIN
    trace_msg( p_text );
    IF p_asp.FIRST IS NOT NULL AND p_asp.LAST IS NOT NULL THEN
      FOR i IN p_asp.FIRST..p_asp.LAST LOOP
        print_one_airspace( p_asp(i) );
      END LOOP;
    END IF;
  EXCEPTION
    WHEN OTHERS THEN
      accom.log_error_msg(
           put_file_handler     => p_Err_Report,
           output_line          => 'ORA Error ' ||
                                   SQLCODE || ' ' || SQLERRM ||
                                   '$' || p_asp.first || '$' || p_asp.last,
           logging_file_handler => p_Gen_Report,
           calling_routine      => Subprogram_Name,
           Error_Id             => 'TRANS0000',
           Error_Code           => accom.g_rc_error,
           Rc                   => rc );
  END print_airspace_list;
  ------------------------------------------------------------------------------
  -- Function   : type_to_text
  -- Description: Given the location type, return text for message.
  ------------------------------------------------------------------------------
  FUNCTION type_to_text (
       p_type   IN     loc_ahi_spec.expiration_location_type%TYPE
  ) RETURN VARCHAR2 IS
  BEGIN
    RETURN
      CASE p_type
        WHEN 'FIX_NAME'  THEN 'fix'
        WHEN 'ATC_POINT' THEN 'ATC point'
        WHEN 'AIRPORT'   THEN 'airport'
        ELSE                  '????????'
      END;
  END type_to_text;
  ------------------------------------------------------------------------------
  -- Procedure  : populate_ahi_spec
  -- Description: Fill record of TYPE ahi_spec_type for internal processing.
  --              Note that not all record fields, such as route id, are filled.
  ------------------------------------------------------------------------------
  PROCEDURE populate_ahi_spec (
       p_in    IN     loc_ahi_spec%ROWTYPE,
       p_out   IN OUT ahi_spec_type
  ) IS
  BEGIN
    p_out.alt.lower         := p_in.applicable_lower_alt;
    p_out.alt.upper         := p_in.applicable_upper_alt;

    p_out.ext.loc.rte       := p_in.route_id;
    p_out.ext.loc.key       := '5';
    p_out.ext.loc.type      := CASE p_in.init_loc_is_atc_point
                                 WHEN 'T' THEN 'ATC_POINT'
                                 ELSE          'FIX_NAME' END;
    p_out.ext.loc.name      := p_in.initiation_location;
    p_out.ext.loc.fix_id    := p_in.init_fix_id;
    p_out.ext.loc.icao_code := p_in.init_icao_code;
    p_out.ext.name          := p_in.exited_airspace;
    
    p_out.ent.loc.rte       := p_in.route_id;
    p_out.ent.loc.key       := '6';
    p_out.ent.loc.type      := p_in.expiration_location_type;
    p_out.ent.loc.name      := p_in.expiration_location;
    p_out.ent.loc.fix_id    := p_in.exp_fix_id;
    p_out.ent.loc.icao_code := p_in.exp_icao_code;
    p_out.ent.name          := p_in.ahi_target;
  END populate_ahi_spec;
  ------------------------------------------------------------------------------
  -- Function   : location_info
  -- Description: Given the location, return text which describes it:
  --              initiation/expiration, location type, location name and,
  --              if location is a fix, the fix ID and ICAO code.
  ------------------------------------------------------------------------------
  FUNCTION location_info (
       p_loc    IN     ahi_object_type
  ) RETURN VARCHAR2 IS
  BEGIN
    RETURN
      CASE p_loc.type
        WHEN 'FIX_NAME' THEN (CASE p_loc.key WHEN '5' THEN 'initiation '
                                             WHEN '6' THEN 'expiration '
                                             ELSE          '????? ' END)||
                             type_to_text( p_loc.type ) || ' ' ||
                             p_loc.name      || ' ('||
                             p_loc.fix_id    || '.' ||
                             p_loc.icao_code || ')'
        ELSE                 (CASE p_loc.key WHEN '5' THEN 'initiation'
                                             WHEN '6' THEN 'expiration'
                                             ELSE          '?????' END)||' '||
                             type_to_text( p_loc.type ) || ' ' ||
                             p_loc.name
      END;
  END location_info;
  ------------------------------------------------------------------------------
  -- Function   : ahi_spec_desc
  -- Description: Return description for the AHI Specifier
  --
  -- Inputs     :
  --   p_type   - FIX_NAME, ATC_POINT or AIRPORT
  --
  -- Outputs    :
  --   Returns "fix", "ATC point" or 'airport".
  ------------------------------------------------------------------------------
  FUNCTION ahi_spec_desc (
       p_route_id  IN  loc_ahi_spec.route_id%TYPE,
       p_enter     IN  ahi_airspace_type,
       p_exit      IN  ahi_airspace_type,
       p_alt       IN  ahi_alt_range%TYPE
  ) RETURN VARCHAR2 IS
  BEGIN
    RETURN p_ACES_route_type || ' ' || p_route_id  || ', ' ||
           location_info(p_enter.loc)||' AHI target ' ||p_enter.name||', '||
           location_info(p_exit.loc )||' exited airspace ' ||p_exit.name ||
           ', applicable altitude range ' ||
           to_char(p_alt.lower) || '-' || to_char(p_alt.upper);
  END ahi_spec_desc;
  ------------------------------------------------------------------------------
  -- Function   : norm_alt
  -- Description: Normalize altitude. If 999, return 999, otherwise add 1.
  ------------------------------------------------------------------------------
  FUNCTION norm_alt (
       p_alt   IN loc_ahi_spec.applicable_upper_alt%TYPE
  ) RETURN  loc_ahi_spec.applicable_upper_alt%TYPE IS
  BEGIN
    RETURN CASE p_alt
             WHEN 999 THEN 999
             ELSE          p_alt + 1 END;
  END norm_alt;
  --
  ------------------------------------------------------------------------------
  -- Function   : alt_range_overlap
  -- Description: Determine if two altitude ranges overlap.
  -- Inputs     : altitude ranges p_alt_1 and p_alt_2
  -- Return     : TRUE if overlapped, otherwise FALSE.
  ----------------------------------------------------------------------------
  FUNCTION alt_range_overlap (
       p_alt_1 IN ahi_alt_range%TYPE,
       p_alt_2 IN ahi_alt_range%TYPE
  ) RETURN BOOLEAN IS
  BEGIN
    RETURN p_alt_1.lower < p_alt_2.upper AND
           p_alt_2.lower < p_alt_1.upper;
  END alt_range_overlap;
  --
  ------------------------------------------------------------------------------
  -- Function   : loc_alt_range_overlap
  -- Description: Determine if the two altitude ranges following the same
  --              5- or 6-key (of the same location) overlap.
  --              The altitude ranges were fetched in the order of the lower
  --              altitude, so the current altitude range of the same location
  --              is expected to be higher that the previous one.
  --
  -- Inputs     :
  --   p_loc_prev - previous location data
  --   p_loc_curr - current  location data
  --
  -- Return     :
  --   TRUE if overlapped, otherwise FALSE.
  ----------------------------------------------------------------------------
  FUNCTION loc_alt_range_overlap (
       p_loc_prev IN OUT ahi_object_type,
       p_loc_curr IN     ahi_object_type
  ) RETURN BOOLEAN IS
    rc        accom.completion_code%TYPE;
    overlap   BOOLEAN := FALSE;
  BEGIN
    IF p_loc_prev.rte  IS NOT NULL AND
       p_loc_prev.key  IS NOT NULL AND
       p_loc_prev.type IS NOT NULL AND
       p_loc_prev.name IS NOT NULL AND
       p_loc_curr.rte  = p_loc_prev.rte  AND
       p_loc_curr.key  = p_loc_prev.key  AND
       p_loc_curr.type = p_loc_prev.type AND
       p_loc_curr.name = p_loc_prev.name AND
       alt_range_overlap( p_loc_prev.alt, p_loc_curr.alt ) THEN
      --------------------------------------------------------
      -- Overlapped alt range is an unexpected error
      --------------------------------------------------------
      accom.log_error_msg(
           put_file_handler     => p_Err_Report,
           output_line          =>
                p_ACES_route_type || ' ' || p_loc_curr.rte || ' ' ||
                location_info( p_loc_curr ) ||
                ', an overalp in altitude ranges between '||
                to_char(p_loc_prev.alt.lower)||'-'||
                to_char(p_loc_prev.alt.upper)||' and '||
                to_char(p_loc_curr.alt.lower)||'-'||
                to_char(p_loc_curr.alt.upper)||
                ' is detected. AHI route not transitioned.',
           logging_file_handler => p_Gen_Report,
           calling_routine      => Subprogram_Name,
           Error_Id             => 'TRANS0738',
           Error_Code           => accom.g_rc_warning,
           Rc                   => rc );
      overlap := TRUE;
    END IF;
    ----------------------------
    -- Save for next verify
    ----------------------------
    p_loc_prev := p_loc_curr;
    RETURN overlap;
  END loc_alt_range_overlap;
  ------------------------------------------------------------------------------
  -- Procedure  : verify_location
  -- Description: Given the parent key character and the location ID, determine
  --              the location type.
  --              This function is called in the second pass of transition
  --              where a location always exists; if it does not raise system
  --              error.
  --
  -- Inputs     :
  --   p_parent   - parent key character: 'F' for fix/ATC point, 'A' for airport
  --   p_loc_id   - Fix ID, ATC point ID or airport ID
  --   p_location - location data.
  --   rc_com     - return code
  --
  -- Outputs    :
  --   If no error, p_location type and name will be updated.
  --   If location is an has airport alias, it will be the location name.
  ----------------------------------------------------------------------------
  PROCEDURE verify_location (
       p_route_id    IN     aces_route.route_id%TYPE,
       p_parent      IN     VARCHAR2,
       p_loc_id      IN     aces_route_fix.fix_name%TYPE,
       p_key         IN     VARCHAR2,
       p_location    IN OUT ahi_object_type,
       rc            IN OUT accom.completion_code%TYPE    
  ) IS

    Subprogram_Name     VARCHAR2(50) := 'actrans_ahi.verify_location';
    loc_exists          PLS_INTEGER := 0;
    unique_icao_code    mrg_airport.icao_code%TYPE;

  BEGIN
    p_location.rte       := p_route_id;
    p_location.name      := p_loc_id;
    p_location.alias     := p_loc_id;
    p_location.key       := p_key;
    p_location.fix_id    := NULL;
    p_location.icao_code := NULL;
    ----------------------------------------------------------------------------
    -- If parent key char is 'F', location is expected to be a fix or ATC point.
    --   If location is found in MRG_FIX and unqiue fix is defined in ACES_FIX,
    --                                          type is FIX_NAME.
    --   If location is found in LOC_ATC_POINT, type is ATC_POINT.
    --   Otherwise, raise exception to log warning.
    -- If parent key char is 'A', location is expected to be an airport.
    --   If location is found in LOC_AIRPORT,   type is AIRPORT.
    --   Otherwise, raise exception to log warning.
    ----------------------------------------------------------------------------
    IF p_parent = 'F' THEN

      BEGIN
        SELECT M.fix_id, M.icao_code
         INTO  p_location.fix_id, p_location.icao_code
         FROM  aces_fix A, mrg_fix M
         WHERE A.version   = p_ACES_Version
           AND M.version   = p_Local_Version
           AND M.fix_name  = p_loc_id
           AND A.fix_name  = M.fix_name
           AND A.fix_id    IS NOT NULL
           AND A.icao_code IS NOT NULL
           AND M.fix_id    = A.fix_id
           AND M.icao_code = A.icao_code;     

         p_location.type := 'FIX_NAME';

      EXCEPTION
        WHEN NO_DATA_FOUND THEN
          ------------------------------------------------------
          -- Location not a fix, see if it is an ATC point
          ------------------------------------------------------
          SELECT count(*) INTO loc_exists
           FROM  loc_atc_point
           WHERE version = p_Local_Version
            AND point_id = p_loc_id;

          IF loc_exists > 0 THEN
            p_location.type := 'ATC_POINT';
          ELSE
            RAISE ahi_error;
          END IF;

      END;

    ELSE  -- p_parent = 'A'
      ----------------------------------------------------------
      -- Verify that location is an airport or if a valid airport
      -- that translates it to an airport id
      ----------------------------------------------------------
      actrans_common.Get_Apt_Alias_And_ICAO_Code(
           p_Local_Version   => p_Local_Version,
           p_Airport_Id      => p_location.name,
           p_Item_Id         => p_route_id,
           p_Item_Type       => p_Local_route_type,
           p_error_file      => p_Err_Report,
           p_gen_error       => p_Gen_Report,
           rc_Alias_Icao     => rc,
           unique_icao       => unique_icao_code );

      IF unique_icao_code IS NOT NULL THEN
        p_location.type := 'AIRPORT';
      ELSE
        RAISE ahi_error;
      END IF;

    END IF;

  EXCEPTION
    WHEN ahi_error THEN
      --------------------------------------------------------------------------
      -- If location does not exist, do not transition AHI route
      --------------------------------------------------------------------------
      accom.log_error_msg(
           put_file_handler     => p_Err_Report,
           output_line          =>
                p_ACES_route_type || ' ' || p_location.rte || ' ' ||
                p_parent||'-key location '||p_location.name||
                ' is not adapted. AHI route not transitioned',
           logging_file_handler => p_Gen_Report,
           calling_routine      => Subprogram_Name,
           Error_Id             => 'TRANS0737',
           Error_Code           => accom.g_rc_warning,
           Rc                   => rc_com);
      RAISE ahi_error;
  END verify_location;
  --
  ------------------------------------------------------------------------------
  -- Function   : get_airspaces
  -- Description:
  --   Given a location's altitude range, this function produces a list of
  --   airspaces (FAV/ARTCC) that together encompass that range.
  --   First the function searches V-key following P-key of the location
  --     ACES_RTE_FIX_POSTING_FPA - if parent is F-key
  --     ACES_RTE_ARR_POSTING_FPA - if parent is A-key
  --     if not found, it searches ERAM alt stratification data in
  --       MRG_FIX_OVER_SPACE  - if location is a fix 
  --       LOC_ATC_OVER_SPACE  - if location is an ATC point
  --       LOC_ARPR_OVER_SPACE - if location is an airport
  --   If location is an expiration location (6-key), both FAVs and ARTCCs are
  --   included in airspace list.
  --   If location is an initiation location (5-key), only FAVs are included in
  --   the airspace list.
  --   Note that except for alt of 999, location and V-key upper
  --   altitudes are 1 less than the actual value.
  --   Note that when a V-key airspace is less than 4 characters it is assumed
  --   to be an ARTCC and not altered. Otherwise, it is assumed to be an FAV
  --   a '0' is prepended to it.
  -- Detailed algorithm:
  --   AHI transition will try to use as much information in the V-keys as
  --   possible before it looks up the ERAM altitude stratification tables.
  --   1. For ALL V-key altitudes of a location
  --     a. If V-key airspace defined
  --       i. If location is init loc (5-key) and airspace is ARTCC
  --         1. Log warning and discard airspace. A gap exists in alt ranges.
  --       ii. Otherwise
  --         1. Accept airspace for the location.
  --     b. Otherwise, V-key airspace not defined
  --       i. Search for ALL alt ranges in ERAM alt strat that covers the V-key
  --          alt range.
  --         1. If there is an overlap between a V-key alt range and an ERAM alt
  --            strat record, trim airspace alt range as follows:
  --           a. If overlap, the lower alt of the lowest airspace is set to
  --              the lower alt of the V-key (upper alt of the previous V-key).
  --           b. If overlap, the upper alt range of the highest airspace is
  --               set to the upper alt of the V-key.
  --         2. If there is a gap between the V-key alt range and an ERAM alt
  --            strat record, or between two ERAM alt strat records
  --           a. Leave the gap as is. The final airspace list will have a gap.
  --         3. If location is initiation location (5-key) and airspace is ARTCC
  --           a. Log warning and discard airspace. A gap exists in alt ranges.
  --         4. Otherwise
  --           a. Accept airspace for the location.
  --   2. If there are no more V-key alt ranges but the location's alt range is
  --      not entirely covered (which includes the case where there are no
  --      v_keys at all for the location).
  --     a. For ALL ERAM alt strat records of the location starting with highest
  --        upper alt from the V-key fetch.
  --       i. If there is an overlap between a V-key alt range and an ERAM alt
  --         strat record, trim airspace alt range as follows:
  --         1. If overlap, the lower alt of the lowest airspace is set to the
  --            lower alt of the V-key (upper alt of the previous V-key).
  --         2. If overlap, the upper alt of the highest airspace is set to the
  --            upper alt of the V-key.
  --       ii. If there is a gap between the V-key altitude range and an ERAM
  --           alt strat record, or between two ERAM alt strat records
  --         1. Leave the gap as is. The final airspace list will have a gap.
  --       iii. If location is initiation location (5-key) and airspace is ARTCC
  --         1. Log warning and discard airspace. A gap exists in alt ranges.
  --       iv. Otherwise
  --         1. Accept airspace for the location.
  --
  -- Inputs     :
  --   p_alt_lower  - the initial lower altitude to be used to start the search
  --   p_location   - location name, type (FIX_NAME, ATC_POINT or AIRPORT),
  --                  altitude range, etc.
  --   p_airspace   - output array of FAV/ARTCCs, each with an altitude range
  --   rc           - return code
  --
  -- Outputs    :
  --   Returns the number of airspaces.
  ------------------------------------------------------------------------------
  FUNCTION get_airspaces(
       p_location    IN     ahi_object_type,
       p_airspace    IN OUT ahi_airspace_array,
       rc            IN OUT accom.completion_code%TYPE
  ) RETURN PLS_INTEGER IS

    Subprogram_Name VARCHAR2(50) := 'actrans_ahi.get_airspaces';
    asp             ahi_airspace_type;
    asp_count       PLS_INTEGER  := nvl(p_airspace.LAST,0);
    asp_count_save  PLS_INTEGER  := nvl(p_airspace.LAST,0);
    l_vkey_exists   BOOLEAN      := FALSE;
    s_cur           cur_type;
    l_alt           ahi_alt_range%TYPE;
    ------------------------------------------------------------
    -- Upper alt limit for ERAM alt strat range search when V-key
    -- airspace is missing.
    ------------------------------------------------------------
    l_vstrat_limit  loc_ahi_spec.applicable_upper_alt%TYPE;
    --

    ----------------------------------------------------------------------------
    -- Function   : check_ERAM_Overlap
    -- Description: Determine if the ERAM airspace have altitude overlap.
    --              Return TRUE to stop processing of get_airspace.
    ----------------------------------------------------------------------------
    FUNCTION check_Eram_Overlap(
         p_location    IN     ahi_object_type
         
    ) RETURN BOOLEAN IS
      overlap_found    BOOLEAN := FALSE;
      rc_convert      accom.completion_code%TYPE := accom.g_rc_no_error;
      OVSArray         Overspace_Array_t;
      rc              accom.completion_code%TYPE;
    BEGIN
      
      CASE p_location.type
        WHEN 'FIX_NAME' THEN        
         SELECT fix_id,icao_code,
           lower_alt, upper_alt,fav_or_aor,module_id
           bulk collect into OVSArray
           from mrg_fix_over_space
           where version = p_Local_Version
           and fix_id    = p_location.fix_id
           and icao_code = p_location.icao_code
           order by lower_alt,upper_alt;

        WHEN 'ATC_POINT' THEN       
	  SELECT point_id,'--',
             lower_alt, upper_alt,fav_or_aor,module_id
             bulk collect into OVSArray
             from loc_atc_over_space
             where version = p_Local_Version
             and point_id   =p_location.name
             order by lower_alt,upper_alt;

        WHEN 'AIRPORT' THEN
	   SELECT airport_id,'--',
            AIRSPACE_LWR_ALT, AIRSPACE_UPPR_ALT,FAV_ID,module_id
            bulk collect into OVSArray
            from loc_arpt_over_space
            where version = p_Local_Version
            and airport_id  = p_location.name
	    order by AIRSPACE_LWR_ALT,AIRSPACE_UPPR_ALT;

       END CASE;
      
       for i in 1..OVSArray.count loop
         for j in i+1..OVSArray.count loop
         -------------------------------------------------------------
         -- check for overlap.
         -------------------------------------------------------------
         --  
           if not (OVSArray(J).lower_alt >= OVSArray(i).upper_alt or
              OVSArray(J).upper_alt <= OVSArray(i).lower_alt) then  

              accom.log_error_msg(
                put_file_handler     => p_Err_Report,
                output_line          =>
                p_ACES_route_type ||' '|| p_location.rte ||' '||
                location_info(p_location) ||
                ' overlap in altitude ranges between '||
                'airspace '||OVSArray(J).fav_or_aor||' '||
                OVSArray(J).lower_alt||'-'||
                OVSArray(J).upper_alt||' and '||
                'airspace '||OVSArray(i).fav_or_aor||' '||
                OVSArray(i).lower_alt||'-'||
                OVSArray(i).upper_alt||
                '. AHI route not transitioned.',
                logging_file_handler => p_Gen_Report,
                calling_routine      => Subprogram_Name,
                Error_Id             => 'TRANS0754',
                Error_Code           => accom.g_rc_warning,
                Rc                   => rc );
                overlap_found := TRUE;   
              end if;
           end loop; -- j
         end loop; -- i

         RETURN  overlap_found;
    END check_ERAM_overlap;

    ----------------------------------------------------------------------------
    -- Function   : process_alt
    -- Description: Determine if the feteched airspace is included in the list.
    --              Return TRUE to continue to fetch in the current loop.
    ----------------------------------------------------------------------------
    FUNCTION process_alt(
         p_upper_limit    IN   loc_ahi_spec.applicable_upper_alt%TYPE,
         p_src            IN   VARCHAR2
    ) RETURN BOOLEAN IS
      continue_loop   BOOLEAN := TRUE;
      rc_convert      accom.completion_code%TYPE := accom.g_rc_no_error;
    BEGIN
      IF asp.alt.lower >= p_upper_limit THEN
        ------------------------------------------------------------------------
        -- The fetched range does not cover the input range.
        -- Mostly likely there is a gap in the alt stratification data.
        -- Next low alt fetch target is the upper limit.
        -- End of fetch.
        ------------------------------------------------------------------------
        continue_loop := FALSE;
        l_alt.lower := p_upper_limit;
      ELSE
        IF asp.alt.upper >= p_upper_limit THEN
          --------------------------------------------------------------
          -- FAV upper alt range covers exactly or more alt than necessary.
          -- Trim upper alt to limit.
          -- Altitude range covered, so exit loop.
          --------------------------------------------------------------
          asp.alt.upper := p_upper_limit;
          continue_loop := FALSE;
        END IF;
        IF asp.alt.lower <= l_alt.lower THEN
          --------------------------------------------------------------
          -- ERAM alt stratification range can have a lower altitude
          -- lower than needed. Trim to take the upper part of the range.
          --------------------------------------------------------------
          asp.alt.lower := l_alt.lower;
        END IF;
        ----------------------------------------------------------------
        -- Next lower alt fetch target is current upper alt.
        ----------------------------------------------------------------
        l_alt.lower := asp.alt.upper;
        IF asp.alt.upper > l_alt.upper THEN
          -- save current upper altitude
          l_alt.upper := asp.alt.upper;
        END IF;
        ----------------------------------------------------------------
        -- If source of airspace name is V-key an it is an ARTCC,
        -- convert computer ID to facility ID.
        -- If conversion error, discard airspace.
        ----------------------------------------------------------------
        IF p_src = 'V' AND length(asp.name) <= 3 THEN
          asp.name := actrans_common.computer_id_to_facility_id(
                           p_Local_Version     => p_Local_Version,
                           p_computer_id       => asp.name,
                           Data_Set            => location_info( p_location )||
                                                  ' V-key ',
                           Subprogram_Name     => Subprogram_Name, 
                           Transition_Error    => p_Err_Report,
                           Report_File_Handler => p_Gen_Report,
                           Rc_CFacility        => rc_convert );
        END IF;
        IF rc_convert >= accom.g_rc_error THEN
          rc := rc_convert;
        ELSE
          --------------------------------------------------------------
          -- Verify data before adding airspace to list
          --------------------------------------------------------------
          IF p_location.key = '5' AND length(asp.name) <= 3 THEN
            --------------------------------------------------
            -- For exited airspace (5-key), ARTCC not allowed.
            -- Log warning and do not enter airspace into list.
            --------------------------------------------------
            accom.log_error_msg(
                  put_file_handler     => p_Err_Report,
                  output_line          =>
                    p_ACES_route_type || ' ' || p_location.rte || ' ' ||
                    location_info( p_location ) ||', altitude range ' ||
                    to_char(p_location.alt.lower)|| '-' ||
                    to_char(p_location.alt.upper)||', the airspace above '||
                    p_location.name || ' for altitude range '||
                    to_char(asp.alt.lower) || '-'||
                    to_char(asp.alt.upper) ||
                     ' is an ARTCC ('||asp.name||'). Data discarded.',
                    logging_file_handler => p_Gen_Report,
                  calling_routine      => Subprogram_Name,
                  Error_Id             => 'TRANS0742',
                  Error_Code           => accom.g_rc_warning,
                  Rc                   => rc );
          ELSE
            ----------------------------------------------------
            -- Include airspace in list.
            ----------------------------------------------------
            p_airspace.EXTEND;
            asp.loc   := p_location;
            asp_count := asp_count + 1;
            p_airspace(asp_count) := asp;
          END IF;           -- Error in the airspace data?
        END IF;             -- If facility ID conversion error?
      END IF;               -- Fetched range covers input range?
      RETURN continue_loop;
    END process_alt;

  BEGIN                              -- *** Start of process_airspaces() ***
    l_alt.lower   := p_location.alt.lower;
    l_alt.upper   := 0;
    if check_eram_overlap(p_location => p_location) = False then

    ----------------------------------------------------------------------------
    -- Search PxR V-key following P-key for altitude range and airspace
    -- If not found, query ERAM altitude stratification.
    -- Lower limit of altitude range is the lower alt of first record
    ----------------------------------------------------------------------------
    IF p_location.type = 'AIRPORT' THEN
      --------------------------------------------------------------------------
      -- Airport. Query ACES_ARR_FIX_POSTING_ALT
      --   If FPA/ARTCC not found in ACES_ARR_FIX_POSTING_FPA
      --     query ERAM altitude stratification LOC_ARPT_OVER_SPACE
      -- Note that p_location.alias contains the ACES airport ID or alias
      --       and p_location.fix_name contains the local airport ID.
      -- The primary FPA has the highest sequence number.
      --------------------------------------------------------------------------
      FOR A_P_alt IN (
        SELECT *
         FROM  aces_rte_arr_posting_alt
         WHERE version         = p_ACES_Version
           AND route_type      = p_ACES_route_type
           AND route_id        = p_location.rte
           AND airport_id      = p_location.alias
           AND upper_altitude >= p_location.alt.lower
        ORDER BY upper_altitude
      ) LOOP

        l_vkey_exists := TRUE;

        BEGIN
          SELECT CASE WHEN length(fpa_or_adj_ctr) < 4 THEN fpa_or_adj_ctr
                      ELSE                          '0' || fpa_or_adj_ctr END,
                 'V', l_alt.lower,
                 decode(A_P_alt.upper_altitude,999,999,
                        A_P_alt.upper_altitude+1)
           INTO  asp.name, asp.src, asp.alt.lower, asp.alt.upper
           FROM  aces_rte_arr_posting_fpa
           WHERE version        = p_ACES_Version
             AND route_type     = p_ACES_route_type
             AND route_id       = p_location.rte
             AND airport_id     = A_P_alt.airport_id
             AND posting_code   = A_P_alt.posting_code
             AND upper_altitude = A_P_alt.upper_altitude
             AND fpa_seq = ( SELECT MAX(fpa_seq)
                              FROM  aces_rte_arr_posting_fpa
                              WHERE version        = p_ACES_Version
                                AND route_type     = p_ACES_route_type
                                AND route_id       = p_location.rte
                                AND airport_id     = A_P_alt.airport_id
                                AND posting_code   = A_P_alt.posting_code
                                AND upper_altitude = A_P_alt.upper_altitude );

          EXIT WHEN NOT process_alt( p_location.alt.upper, 'V' );

        EXCEPTION
          WHEN NO_DATA_FOUND THEN
            --------------------------------------------------------------------
            -- If no more V-key FPAs within the location's altitude range,
            -- move on to ERAM altitude stratification.
            -- Get one or more Alt Strat altitude ranges that cover up to
            -- the V-key altitude range, starting with the max lower alt that
            -- encompass the input alt range.
            -- The upper limit to process_alt() is the lower of
            --   - the input location's upper altitude (may have been truncated)
            --   - the ERAM alt strat upper altitude
            --------------------------------------------------------------------
            l_vstrat_limit :=
              CASE WHEN p_location.alt.upper > norm_alt(A_P_alt.upper_altitude)
                     THEN norm_alt(A_P_alt.upper_altitude)
                     ELSE p_location.alt.upper
              END;
            OPEN s_cur FOR
              SELECT fav_id, 's', trunc(airspace_lwr_alt/100),
                                  trunc(airspace_uppr_alt/100)
               FROM  loc_arpt_over_space A
               WHERE A.version           = p_Local_Version
                 AND A.airport_id        = p_location.name
                 AND A.airspace_lwr_alt >= (
                       SELECT MAX(airspace_lwr_alt) FROM loc_arpt_over_space B
                        WHERE B.version            = p_Local_Version
                          AND B.airport_id         = p_location.name
                          AND B.airspace_lwr_alt  <= l_alt.lower*100 )
                   AND A.airspace_uppr_alt > l_alt.lower*100
              ORDER BY A.airspace_lwr_alt, A.airspace_uppr_alt;
            LOOP
              FETCH s_cur INTO asp.name, asp.src, asp.alt.lower, asp.alt.upper;
              EXIT WHEN s_cur%NOTFOUND OR NOT process_alt(l_vstrat_limit, 'S');
            END LOOP;
            CLOSE s_cur;
        END;                -- FAV/ARTCC not found in V-key
      END LOOP;             -- loop aces_rte_arr_posting_alt
    ELSE                    -- p_location.type in ('FIX_NAME', 'ATC_POINT')
      --------------------------------------------------------------------------
      -- Fix or ATC point. Query ACES_RTE_FIX_POSTING_ALT, _FPA
      --   If FPA/ARTCC not found in ACES_RTE_FIX_POSTING_FPA
      --     query ERAM altitude stratification MRG_FIX_ or LOC_ATC_OVER_SPACE.
      -- The primary FPA has the highest sequence number.
      --------------------------------------------------------------------------
      FOR F_P_alt IN (
        SELECT *
         FROM  aces_rte_fix_posting_alt
         WHERE version         = p_ACES_Version
           AND route_type      = p_ACES_route_type
           AND route_id        = p_location.rte
           AND fix_name        = p_location.name
           AND seq_num         = p_location.seqn  
           AND upper_altitude >= p_location.alt.lower
        ORDER BY upper_altitude
      ) LOOP

        l_vkey_exists := TRUE;

        BEGIN
          SELECT CASE WHEN length(fpa_or_adj_ctr) < 4 THEN fpa_or_adj_ctr
                      ELSE                          '0' || fpa_or_adj_ctr END,
                 'V', l_alt.lower,
                 decode(F_P_alt.upper_altitude,999,999,
                        F_P_alt.upper_altitude+1)
           INTO  asp.name, asp.src, asp.alt.lower, asp.alt.upper
           FROM  aces_rte_fix_posting_fpa
           WHERE version         = p_ACES_Version
             AND route_type      = p_ACES_route_type
             AND route_id        = p_location.rte
             AND fix_name        = F_P_alt.fix_name
             AND seq_num         = F_P_alt.seq_num         
             AND posting_code    = F_P_alt.posting_code
             AND upper_altitude  = F_P_alt.upper_altitude
             AND fpa_seq = ( SELECT MAX(fpa_seq)
                              FROM  aces_rte_fix_posting_fpa
                              WHERE version        = p_ACES_Version
                                AND route_type     = p_ACES_route_type
                                AND route_id       = p_location.rte
                                AND fix_name       = F_P_alt.fix_name
                                AND seq_num        = F_P_alt.seq_num
                                AND posting_code   = F_P_alt.posting_code
                                AND upper_altitude = F_P_alt.upper_altitude );

          EXIT WHEN NOT process_alt( p_location.alt.upper, 'V' );

        EXCEPTION
          WHEN NO_DATA_FOUND THEN
            --------------------------------------------------------------------
            -- If no more V-key FPAs within the location's altitude range,
            -- move on to ERAM altitude stratification.
            -- Get one or more Alt Strat altitude ranges that cover up to
            -- the V-key altitude range, starting with the max lower alt that
            -- encompass the input alt range.
            -- The upper limit to process_alt() is the lower of
            --   - the input location's upper altitude (may have been truncated)
            --   - the ERAM alt strat upper altitude
            --------------------------------------------------------------------
            l_vstrat_limit :=
              CASE WHEN p_location.alt.upper > norm_alt(F_P_alt.upper_altitude)
                     THEN norm_alt(F_P_alt.upper_altitude)
                     ELSE p_location.alt.upper
              END;
            IF p_location.type = 'FIX_NAME' THEN    
              OPEN s_cur FOR
                SELECT fav_or_aor, 's',
                       trunc(lower_alt/100), trunc(upper_alt/100)
                 FROM  mrg_fix_over_space A
                 WHERE A.version   = p_Local_Version
                   AND A.fix_id    = p_location.fix_id
                   AND A.icao_code = p_location.icao_code
                   AND A.lower_alt  >= (
                         SELECT MAX(B.lower_alt) FROM mrg_fix_over_space B
                          WHERE B.version     = p_Local_Version
                            AND B.fix_id      = p_location.fix_id
                            AND B.icao_code   = p_location.icao_code
                            AND B.lower_alt  <= l_alt.lower*100 )
                   AND A.upper_alt > l_alt.lower*100
                ORDER BY A.lower_alt, A.upper_alt;
            ELSE   -- p_location_type = 'ATC_POINT'
              OPEN s_cur FOR
                SELECT fav_or_aor, 's',
                       trunc(lower_alt/100), trunc(upper_alt/100)
                 FROM  loc_atc_over_space A
                 WHERE A.version    = p_Local_Version
                   AND A.point_id   = p_location.name
                   AND A.lower_alt >= (
                         SELECT MAX(B.lower_alt) FROM loc_atc_over_space B
                          WHERE B.version    = p_Local_Version
                            AND B.point_id   = p_location.name
                            AND B.lower_alt <= l_alt.lower*100 )
                   AND A.upper_alt > l_alt.lower*100
                ORDER BY A.lower_alt, A.upper_alt;
            END IF;
 
            LOOP
              FETCH s_cur INTO asp.name, asp.src, asp.alt.lower, asp.alt.upper;
              EXIT WHEN s_cur%NOTFOUND OR NOT process_alt(l_vstrat_limit, 'S');
            END LOOP;
            CLOSE s_cur;
        END;                -- FAV/ARTCC not found in V-key
      END LOOP;             -- loop acse_rte_fix_posting_alt
    END IF;                 -- Airport or (Fix or ATC point)?

    IF p_location.alt.upper > l_alt.upper THEN
      IF NOT l_vkey_exists THEN
        l_alt.upper := p_location.alt.lower;
      END IF;
      --------------------------------------------------------------------------
      -- Not all altitudes (or none) are covered
      -- Get the rest (or all) of FAV/ARTCCs from ERAM altitude stratification
      -- For a fix        - MRG_FIX_OVER_SPACE
      -- For an ATC point - LOC_ATC_OVER_SPACE
      -- For an airport   - LOC_ARPT_OVER_SPACE
      -- Start from the the highest airspace that covers the location's
      -- lower altitude.
      --------------------------------------------------------------------------
       CASE p_location.type
        WHEN 'FIX_NAME' THEN
          OPEN s_cur FOR
            SELECT trunc(lower_alt/100), trunc(upper_alt/100), fav_or_aor, 'S'
             FROM  mrg_fix_over_space A
             WHERE A.version    = p_Local_Version
               AND A.fix_id     = p_location.fix_id
               AND A.icao_code  = p_location.icao_code
               AND A.lower_alt >= ( SELECT MAX(lower_alt)
                                     FROM  mrg_fix_over_space
                                     WHERE version    = p_Local_Version
                                       AND fix_id     = p_location.fix_id
                                       AND icao_code  = p_location.icao_code
                                       AND lower_alt <= l_alt.upper*100 )
             ORDER BY lower_alt, upper_alt; 
        WHEN 'ATC_POINT' THEN
          OPEN s_cur FOR
            SELECT trunc(lower_alt/100), trunc(upper_alt/100), fav_or_aor, 'S'
             FROM  loc_atc_over_space
             WHERE version     = p_Local_Version
               AND point_id    = p_location.name
               AND lower_alt  >= ( SELECT MAX(lower_alt)
                                    FROM  loc_atc_over_space
                                    WHERE version   = p_Local_Version
                                    AND point_id    = p_location.name
                                    AND lower_alt  <= l_alt.upper*100 )
            ORDER BY lower_alt, upper_alt;
        WHEN 'AIRPORT' THEN
          OPEN s_cur FOR
            SELECT trunc(airspace_lwr_alt/100), trunc(airspace_uppr_alt/100),
                   fav_id, 'S'
             FROM  loc_arpt_over_space
             WHERE version           = p_Local_Version
               AND airport_id        = p_location.name
               AND airspace_lwr_alt >= (
                        SELECT MAX(airspace_lwr_alt)
                          FROM  loc_arpt_over_space
                         WHERE version           = p_Local_Version
                           AND airport_id        = p_location.name
                           AND airspace_lwr_alt <= l_alt.upper*100 )
            ORDER BY airspace_lwr_alt, airspace_uppr_alt;
      END CASE;

      LOOP
        FETCH s_cur INTO asp.alt.lower, asp.alt.upper, asp.name, asp.src;
        EXIT WHEN s_cur%NOTFOUND OR (
                       ( asp.alt.upper > l_alt.upper ) AND
                       NOT process_alt( p_location.alt.upper, 'S' ) );
      END LOOP;
      CLOSE s_cur;
    END IF;

    IF NOT l_vkey_exists AND asp_count = asp_count_save THEN
      ------------------------------------------------
      -- If the lack of airspace data is cause by the
      -- lack of over-space data, log warning.
      ------------------------------------------------
      accom.log_error_msg(
           put_file_handler     => p_Err_Report,
           output_line          =>
                p_ACES_route_type || ' ' || p_location.rte || ' '  ||
                location_info( p_location ) || ', altitude range ' ||
                to_char(p_location.alt.lower) || '-' ||
                to_char(p_location.alt.upper) ||
      ' has no associated altitude stratification information. Data discarded.',
                logging_file_handler => p_Gen_Report,
                calling_routine      => Subprogram_Name,
                Error_Id             => 'TRANS0741',
                Error_Code           => accom.g_rc_warning,
                Rc                   => rc );
    END IF;


   end if; -- if eram fix overlap
    RETURN asp_count;
  EXCEPTION
    WHEN OTHERS THEN
      accom.log_error_msg(
           put_file_handler     => p_Err_Report,
           output_line          => 'ORA Error ' ||
                                   SQLCODE || ' ' || SQLERRM,
           logging_file_handler => p_Gen_Report,
           calling_routine      => Subprogram_Name,
           Error_Id             => 'TRANS0000',
           Error_Code           => accom.g_rc_error,
           Rc                   => rc_com);
      RAISE ahi_error;
  END get_airspaces;
  --
  ------------------------------------------------------------------------------
  -- Function   : validate_ahi_spec
  -- Description:
  --   Given an AHI spec, validate it. External procedure
  --   Transition_AHI_Data_Val() is called to return a list of cross validation
  --   error ID which are then translated into corresponding transition
  --   messages.
  --   Other conditions not validated externally are also done here.
  --
  -- Inputs     :
  --   p_entered  - Entered airspace data
  --   p_exited   - Exited  airspace data
  --   p_alt      - applicable altitude range
  --   p_rc       - return code
  --
  -- Outputs    :
  --   Returns TRUE if the AHI spec is valid NOT to be discarded.
  ----------------------------------------------------------------------------
  FUNCTION validate_ahi_spec (
       p_entered IN     ahi_airspace_type,
       p_exited  IN     ahi_airspace_type,
       p_alt     IN     ahi_alt_range%TYPE,
       p_rc      IN OUT accom.completion_code%TYPE )
  RETURN BOOLEAN IS

    Subprogram_Name VARCHAR2(50) := 'actrans_ahi.validate_ahi_spec';
    l_msg         acloc_cross_val.err_msg_id_array_type;
    l_count       PLS_INTEGER := 0;
    l_trans_msg   trans_msg_type;
    l_valid       BOOLEAN := TRUE;
    l_text        VARCHAR2(600);
    l_fav_alt     ahi_alt_range%TYPE;

  BEGIN
    ----------------------------------------------------------------------------
    -- Get a list of cross validation error codes for this AHI specifier
    ----------------------------------------------------------------------------
    acloc_cross_val_ahi.Transition_AHI_Data_Val (
         p_Local_Version       => p_Local_Version,
         p_Route_Type          => p_Local_route_type,
         p_Route_Id            => p_exited.loc.rte,
         p_Owning_Facility     => p_Facility_ID,
         p_Transition_ID       => l_transition_id,
         p_Lower_Alt           => p_alt.lower,
         p_Upper_Alt           => p_alt.upper,
         p_init_loc_atc_point  => CASE p_exited.loc.type
                                    WHEN 'ATC_POINT' THEN 'T'
                                    ELSE                  'F'
                                  END,
         p_initiation_location => p_exited.loc.name,
         p_init_fix_id         => p_exited.loc.fix_id,
         p_init_icao_code      => p_exited.loc.icao_code,
         p_exited_airspace     => p_exited.name,
         p_exp_loc_type        => p_entered.loc.type,
         p_expiration_location => p_entered.loc.name,
         p_exp_fix_id          => p_entered.loc.fix_id,
         p_exp_icao_code       => p_entered.loc.icao_code,
         p_ahi_target          => p_entered.name,
         p_msg_id              => l_msg,
         p_msg_count           => l_count );
    ----------------------------------------------------------------------------
    -- Translate list of cross validation error codes to transition messages
    ----------------------------------------------------------------------------
    FOR i IN 1..l_count LOOP
      l_trans_msg := 
        CASE l_msg(i)
          --------------------------------------------------------------
          -- Messages for airspaces
          --------------------------------------------------------------    
          WHEN 'CRSLOC1704' THEN
               trans_msg_type( 'TRANS0745', accom.g_rc_warning, 'T',
                               ', exited airspace ' || p_exited.name ||
                               ' is not an adapted FAV.' )
          WHEN 'CRSLOC1708' THEN
               trans_msg_type( 'TRANS0745', accom.g_rc_warning, 'T',
                               ', AHI target ' || p_entered.name ||
                               ' is not an adapted FAV.' )
          WHEN 'CRSLOC1709' THEN
               trans_msg_type( 'TRANS0746', accom.g_rc_warning,   'T',
                               ', AHI target ' || p_entered.name ||
                               ' is not an adapted ARTCC.' )
          WHEN 'CRSLOC1710' THEN
               trans_msg_type( 'TRANS0752', accom.g_rc_warning,   'T',
                               ', exited airspace ' || p_exited.name ||
                               ' is not above initiation fix '||
                               p_exited.loc.name ||
                               ' or any point on the route.' )
          WHEN 'CRSLOC1711' THEN
               trans_msg_type( 'TRANS0752', accom.g_rc_warning,   'T',
                               ', exited airspace ' || p_exited.name ||
                               ' is not above initiation ATC point '||
                               p_exited.loc.name ||
                               ' or any point on the route.' )
          --------------------------------------------------------------
          -- For the rest of the messages, validation has already been done
          -- at an earlier step of transition.
          -- Should not happen. If it does, log as error.
          -- Discard them by default.
          -- (CRSLOC1703/05/06/07)
          --------------------------------------------------------------
          ELSE                trans_msg_type(
                                'TRANS0000', accom.g_rc_error, 'T',
                                '. AHI specifier validation error '||l_msg(i))
        END;
      trace_msg( l_msg(i) );

      IF l_valid THEN
        --------------------------------------------------------
        -- If at least one message indicates an AHI spec is
        -- not valid, discard AHI spec on return.
        --------------------------------------------------------
        l_valid := CASE l_trans_msg(T_DISCARD) WHEN 'T' THEN FALSE
                                               ELSE          TRUE END;
      END IF;        

      IF l_trans_msg(T_ID) IS NOT NULL THEN
        --------------------------------------------------------
        -- Log with transition message ID.
        --------------------------------------------------------
        accom.log_error_msg(
             put_file_handler     => p_Err_Report,
             output_line          =>
                  ahi_spec_desc( p_route_id => p_exited.loc.rte,
                                 p_enter    => p_entered,
                                 p_exit     => p_exited,
                                 p_alt      => p_alt ) ||
                  l_trans_msg(T_TEXT) ||
                  CASE l_trans_msg(T_DISCARD) WHEN 'T' THEN ' Data discarded.'
                                              ELSE ''  END,
             logging_file_handler => p_Gen_Report,
             calling_routine      => Subprogram_Name,
             Error_Id             => l_trans_msg(T_ID),
             Error_Code           => l_trans_msg(T_CODE),
             Rc                   => p_rc );

      END IF;               -- log message?
    END LOOP;               -- For all messages of an AHI spec
    --------------------------------------------------------------------
    -- More validations here. . .
    --------------------------------------------------------------------
    IF p_entered.name = p_exited.name THEN
      ----------------------------------------------------------
      -- Entered airspace = exited airspace, warn and discard
      ----------------------------------------------------------
      accom.log_error_msg(
           put_file_handler     => p_Err_Report,
           output_line          =>
                ahi_spec_desc( p_route_id => p_exited.loc.rte,
                               p_enter    => p_entered,
                               p_exit     => p_exited,
                               p_alt      => p_alt ) ||
                     ', the AHI target is the same as the exited airspace.'||
                     ' Data discarded.',
           logging_file_handler => p_Gen_Report,
           calling_routine      => Subprogram_Name,
           Error_Id             => 'TRANS0748',
           Error_Code           => accom.g_rc_warning,
           Rc                   => p_rc );

      l_valid := FALSE;

    END IF;

    RETURN l_valid;
  END validate_ahi_spec;
  --
  ------------------------------------------------------------------------------
  -- Function   : validate_ahi_spec_post_insert
  -- Description:
  --   Given an AHI spec, validate it. External procedure
  --   Transition_AHI_Data_Post_Val() is called to return a list of AHI specs
  --   against which this AHI spec is validated, and a list of associated cross
  --   validation error ID which are then translated into corresponding
  --   transition messages.
  --   If an error that requires an AHI spec to be discarded, both records are
  --   DELETEd. To avoid duplicate messages, transition message is NOT logged if
  --   both records are deleted. For example AH specs A, B, C in that order
  --   overlaps. In the validation of each pair:
  --     A-B:   A deleted,    B deleted,     message for A-B logged
  --     A-C:   A not found,  C deleted,     message for A-C logged
  --     B-A:   B not found,  A not found,   message for B-A NOT logged  
  --     B-C:   B not found,  C not found,   message for B-C NOT logged  
  --     C-A:   C not found,  A not found,   message for C-A NOT logged  
  --     C-B:   C not found,  B not found,   message for C-B NOT logged  
  --
  -- Inputs     :
  --   p_ahi_spec - AHI specifier record
  --   p_rc       - return code
  --
  -- Outputs    :
  --   Returns TRUE if the AHI spec is valid NOT to be discarded.
  ----------------------------------------------------------------------------
  FUNCTION validate_ahi_spec_post_insert (
       p_ahi_spec      IN     ahi_spec_type,
       p_num_rejected  IN OUT PLS_INTEGER,
       p_rc            IN OUT accom.completion_code%TYPE )
  RETURN BOOLEAN IS

    Subprogram_Name VARCHAR2(50) := 'actrans_ahi.validate_ahi_spec_post_insert';
    l_ahi_spec_rec  loc_ahi_spec%ROWTYPE;
    l_ahi_spec_list acloc_cross_val_ahi.ahi_spec_key_array_type :=
                         acloc_cross_val_ahi.ahi_spec_key_array_type();
    l_ahi_spc2      ahi_spec_type;
    l_msg           acloc_cross_val.err_msg_id_array_type;
    l_count         PLS_INTEGER := 0;
    l_trans_msg     trans_msg_type;
    l_valid         BOOLEAN := TRUE;

  BEGIN
    ----------------------------------------------------------------------------
    -- Get a list of cross validation error codes for this AHI specifier
    ----------------------------------------------------------------------------
    acloc_cross_val_ahi.Transition_AHI_Data_Post_Val (
         p_Local_Version       => p_Local_Version,
         p_Route_Type          => p_Local_route_type,
         p_Route_Id            => p_ahi_spec.ext.loc.rte,
         p_Owning_Facility     => p_Facility_ID,
         p_Transition_ID       => l_transition_id,
         p_Lower_Alt           => p_ahi_spec.alt.lower,
         p_Upper_Alt           => p_ahi_spec.alt.upper,
         p_init_loc_atc_point  => CASE p_ahi_spec.ext.loc.type
                                    WHEN 'ATC_POINT' THEN 'T'
                                    ELSE                  'F'
                                  END,
         p_initiation_location => p_ahi_spec.ext.loc.name,
         p_init_fix_id         => p_ahi_spec.ext.loc.fix_id,
         p_init_icao_code      => p_ahi_spec.ext.loc.icao_code,
         p_exited_airspace     => p_ahi_spec.ext.name,
         p_exp_loc_type        => p_ahi_spec.ent.loc.type,
         p_expiration_location => p_ahi_spec.ent.loc.name,
         p_exp_fix_id          => p_ahi_spec.ent.loc.fix_id,
         p_exp_icao_code       => p_ahi_spec.ent.loc.icao_code,
         p_ahi_target          => p_ahi_spec.ent.name,
         p_ahi_spec_keys       => l_ahi_spec_list,
         p_msg_id              => l_msg,
         p_msg_count           => l_count );
    ----------------------------------------------------------------------------
    -- Translate list of cross validation error codes to transition messages
    ----------------------------------------------------------------------------
    FOR i IN 1..l_count LOOP

      SELECT * INTO l_ahi_spec_rec FROM loc_ahi_spec
       WHERE version              = p_Local_Version
         AND route_type           = p_Local_route_type
         AND route_id             = p_ahi_spec.ext.loc.rte
         AND transition_id        = l_transition_id
         AND applicable_lower_alt = l_ahi_spec_list(i).applicable_lower_alt
         AND applicable_upper_alt = l_ahi_spec_list(i).applicable_upper_alt
         AND initiation_location  = l_ahi_spec_list(i).initiation_location
         AND exited_airspace      = l_ahi_spec_list(i).exited_airspace
         AND expiration_location  = l_ahi_spec_list(i).expiration_location
         AND ahi_target           = l_ahi_spec_list(i).ahi_target;

      populate_ahi_spec( p_in => l_ahi_spec_rec, p_out => l_ahi_spc2 );
      l_trans_msg := 
        CASE l_msg(i)
          ------------------------------------------------------
          -- Translate cross val messages to transition messages
          ------------------------------------------------------
          WHEN 'CRSLOC1720' THEN
               trans_msg_type( 'TRANS0749', accom.g_rc_warning, 'T',
                 p_ACES_route_type || ' ' || p_ahi_spec.ext.loc.rte || ', ' ||
                 location_info(p_ahi_spec.ext.loc)||
                 ' hands off at overlapping altitude ranges from'||
                 ' exited airspace '||p_ahi_spec.ext.name||' to '||      
                 location_info(p_ahi_spec.ent.loc)||
                 ' ahi_target '||p_ahi_spec.ent.name||
                 ', applicable altitude range '||
                 to_char(p_ahi_spec.alt.lower)||'-'||
                 to_char(p_ahi_spec.alt.upper)|| ', and from'||
                 ' exited airspace '||l_ahi_spc2.ext.name||' to '||      
                 location_info(l_ahi_spc2.ent.loc)||
                 ' ahi_target '||l_ahi_spc2.ent.name||
                 ', applicable altitude range '||
                 to_char(l_ahi_spc2.alt.lower)||'-'||
                 to_char(l_ahi_spc2.alt.upper)||'.' )
          --------------------------------------------
          -- For the rest of the messages, do not log.
          -- Discrard them by default.
          --------------------------------------------
          ELSE trans_msg_type( NULL, NULL, 'T', NULL )
        END;

      IF l_trans_msg(T_DISCARD) = 'T' THEN
        --------------------------------------------------------
        -- Discard cursor AHI Spec, if not already deleted
        --------------------------------------------------------
        DELETE FROM loc_ahi_spec
         WHERE version               = p_Local_Version
           AND route_type            = p_Local_route_type
           AND route_id              = p_ahi_spec.ext.loc.rte
           AND applicable_lower_alt  = p_ahi_spec.alt.lower
           AND applicable_upper_alt  = p_ahi_spec.alt.upper
           AND initiation_location   = p_ahi_spec.ext.loc.name
           AND exited_airspace       = p_ahi_spec.ext.name
           AND expiration_location   = p_ahi_spec.ent.loc.name
           AND ahi_target            = p_ahi_spec.ent.name;

        p_num_rejected := p_num_rejected + SQL%ROWCOUNT;

        --------------------------------------------------------
        -- Discard AHI Spec against which cursor spec is compared
        --------------------------------------------------------
        DELETE FROM loc_ahi_spec
         WHERE version               = p_Local_Version
           AND route_type            = p_Local_route_type
           AND route_id              = l_ahi_spc2.ext.loc.rte
           AND applicable_lower_alt  = l_ahi_spc2.alt.lower
           AND applicable_upper_alt  = l_ahi_spc2.alt.upper
           AND initiation_location   = l_ahi_spc2.ext.loc.name
           AND exited_airspace       = l_ahi_spc2.ext.name
           AND expiration_location   = l_ahi_spc2.ent.loc.name
           AND ahi_target            = l_ahi_spc2.ent.name;

        p_num_rejected := p_num_rejected + SQL%ROWCOUNT;

        --------------------------------------------------------
        -- If the record existed and deleted.
        --------------------------------------------------------
        IF SQL%ROWCOUNT > 0 AND l_trans_msg(T_ID) IS NOT NULL THEN
          ------------------------------------------------------
          -- Log with transition message ID.
          ------------------------------------------------------
          accom.log_error_msg(
               put_file_handler     => p_Err_Report,
               output_line          => l_trans_msg(T_TEXT) ||
                    CASE l_trans_msg(T_DISCARD) WHEN 'T' THEN ' Data discarded.'
                                                ELSE ''  END,
               logging_file_handler => p_Gen_Report,
               calling_routine      => Subprogram_Name,
               Error_Id             => l_trans_msg(T_ID),
               Error_Code           => l_trans_msg(T_CODE),
               Rc                   => p_rc );
        END IF;

      END IF;               -- Is invalid data to be discarded?

      IF l_valid THEN
        --------------------------------------------------------
        -- If at least one message indicates an AHI spec is
        -- valid, return is invalid.
        --------------------------------------------------------
        l_valid := CASE l_trans_msg(T_DISCARD) WHEN 'T' THEN FALSE
                                               ELSE          TRUE END;
      END IF;        
    END LOOP;               -- For all messages of an AHI spec
    l_ahi_spec_list.DELETE;
    RETURN l_valid;

  EXCEPTION
    WHEN NO_DATA_FOUND THEN
      ----------------------------------------------------------
      -- AHI spec already discard from the last validation.
      -- Ignore excpetion.
      ----------------------------------------------------------
      l_ahi_spec_list.DELETE;
      RETURN TRUE;
  END validate_ahi_spec_post_insert;
  --
--------------------------------------------------------------------------------
-- *** transition_ahi_pass_2 *** procedure entry
--------------------------------------------------------------------------------
BEGIN
  ----------------------------------------------------------------
 -- If clean transition was selected, then delete all AHIs.
  ----------------------------------------------------------------
  IF p_Clean_Trans_Ind = 'T' THEN
    clean_all_ahis(
         p_Facility_Id   => p_Facility_Id,
         p_Local_Version => p_Local_Version,
         p_Route_Type    => p_Local_route_type,
         p_Err_Report    => p_Err_Report,
         p_Gen_Report    => p_Gen_Report,
         p_rc            => rc_com );
  END IF; -- if clean transition selected
  ----------------------------------------------------------------------------
  -- Get maximum length of comment for generating a new one
  ----------------------------------------------------------------------------
  SELECT DISTINCT data_length INTO comment_length
    FROM user_tab_columns
   WHERE table_name   = 'LOC_AHI_DATA'
     AND column_name  = 'COMMENTS';

  ----------------------------------------------------------------------------
  -- For all ACES routes
  ----------------------------------------------------------------------------
  FOR route IN (
    SELECT * FROM aces_route
     WHERE version    = p_ACES_Version
       AND route_type = p_ACES_route_type
    ORDER BY route_id
  ) LOOP
    --------------------------------------------------------------------
    -- Start Processing of one route
    --------------------------------------------------------------------
    DECLARE
      --
      num_route_6_keys    PLS_INTEGER  := 0;
      l_route_is_valid    varchar2(1)  := 'F';
      ------------------------------------------------------------------
      -- Initiation (5-key) and Expiration (6-key) Locations
      ------------------------------------------------------------------
      l_ini_lst           ahi_object_array := ahi_object_array();
      l_ini_loc           ahi_object_type;
      l_exp_loc           ahi_object_type;
      l_ini_loc_prev      ahi_object_type;
      l_exp_loc_prev      ahi_object_type;
      ------------------------------------------------------------------
      -- Exited (5-key) and Entered (6-key) Airspace (FAV/ARTCC) lists
      ------------------------------------------------------------------
      l_exit              ahi_airspace_array := ahi_airspace_array();
      l_enter             ahi_airspace_array := ahi_airspace_array();
      enter_count         PLS_INTEGER := 0;
      exit_count          PLS_INTEGER := 0;
      exit_start          PLS_INTEGER := 0;
      l_alt               ahi_alt_range%TYPE;
      l_exp_upper_orig    loc_ahi_spec.applicable_upper_alt%TYPE;
      ------------------------------------------------------------------
      -- Variables for inserting alt range into a alt range list.
      ------------------------------------------------------------------
      l_alt_lst           aclocal_util.alt_range_array :=
                               aclocal_util.alt_range_array();
      i_insert            PLS_INTEGER := 0;
      l_gap_found         BOOLEAN     := FALSE;
      ------------------------------------------------------------------
      -- AHI Specifier List for sorting before inserted into database
      ------------------------------------------------------------------
      l_ahi_spec_tmp      ahi_spec_type;
      swapped             BOOLEAN := FALSE;
      --
    BEGIN
      -- debug
      -- ahi_trace := route.route_id IN ( *** enter routes to trace *** );
      ------------------------------------------------------------------------
      -- Determine if AHI Data record for route already exists
      ------------------------------------------------------------------------
      SELECT count(*) INTO ahi_data_count FROM loc_ahi_data
       WHERE version          = p_Local_Version
         AND NVL(owning_facility, ' ')  = NVL(p_Facility_Id, ' ')
         AND route_type       = p_Local_route_type
         AND route_id         = route.route_id;

      --------------------------------------------------------------------------
      -- If route contains a 6-key, keep or create AHI Data record
      -- Otherwise, delete it.
      --------------------------------------------------------------------------
      SELECT count(*) INTO num_route_6_keys
       FROM  ( SELECT DISTINCT fix_name loc FROM aces_rte_fix_6_key
                WHERE version    = p_ACES_Version
                  AND route_type = p_ACES_route_type
                  AND route_id   = route.route_id
               UNION
               SELECT DISTINCT airport_id loc FROM aces_rte_arr_6_key
                WHERE version    = p_ACES_Version
                  AND route_type = p_ACES_route_type
                  AND route_id   = route.route_id );

      IF num_route_6_keys <= 0 THEN
        ------------------------------------------------------------------------
        -- No 6-key for this ACES route.
        -- If Local AHI route exists, mark as Delete to be deleted.
        -- Otherwise, ignore.
        ------------------------------------------------------------------------
        ahi_data_action := CASE WHEN ahi_data_count > 0 THEN ahi_delete
                                                        ELSE ahi_none  END;
      ELSE
        ------------------------------------------------------------------------
        -- A 6-key is found. Verify that route is adpated and valid.
        ------------------------------------------------------------------------
        BEGIN
          SELECT nvl(validated,'F') INTO l_route_is_valid FROM loc_val_route
           WHERE version          = p_Local_Version
             AND NVL(owning_facility, ' ')  = NVL(p_Facility_Id, ' ')
             AND route_type       = p_Local_route_type
             AND route_name       = route.route_id;

          IF l_route_is_valid != 'T' THEN

            accom.log_error_msg(
                 put_file_handler     => p_Err_Report,
                 output_line          =>
                    p_ACES_route_type || ' ' || route.route_id ||
                    ' 6-key exists but route is invalid.',
                 logging_file_handler => p_Gen_Report,
                 calling_routine      => Subprogram_Name,
                 Error_Id             => 'TRANS0744',
                 Error_Code           => accom.g_rc_warning,
                 Rc                   => rc_com );

            ----------------------------------------------------
            -- Bypass 6/5-key transition to discard data
            ----------------------------------------------------
            ahi_data_action := ahi_discard;
            RAISE ahi_error;

          END IF;
        EXCEPTION
          WHEN NO_DATA_FOUND THEN

            accom.log_error_msg(
                 put_file_handler     => p_Err_Report,
                 output_line          =>
                    p_ACES_route_type || ' ' || route.route_id ||
                    ' 6-key exists but route is not adapted.',
                 logging_file_handler => p_Gen_Report,
                 calling_routine      => Subprogram_Name,
                 Error_Id             => 'TRANS0743',
                 Error_Code           => accom.g_rc_warning,

                 Rc                   => rc_com );

            ----------------------------------------------------
            -- Bypass 6/5-key transition to discard data
            ----------------------------------------------------
            ahi_data_action := ahi_discard;
            RAISE ahi_error;

        END;
        ------------------------------------------------------------------------
        -- If 6-key is found, route is adpated and valid,
        -- start transition of AHI Data for route.
        ------------------------------------------------------------------------
        IF l_route_is_valid = 'T' AND ahi_data_count > 0 THEN
          ----------------------------------------------------------------------
          -- Parent record exists in LOC_AHI_DATA.
          -- Mark as update and delete all its exisiting AHI specifiers.
          -- Message is logged after AHI specs, if exist, are transitoned.
          ----------------------------------------------------------------------
          ahi_data_action := ahi_update;

          SELECT comments INTO comment_current
           FROM  loc_ahi_data
           WHERE version         = p_Local_Version
             AND NVL(owning_facility, ' ')  = NVL(p_Facility_Id, ' ')
             AND route_type      = p_Local_route_type
             AND route_id        = route.route_id;
            
          DELETE loc_ahi_spec
           WHERE version         = p_Local_Version
             AND route_type      = p_Local_route_type
             AND route_id        = route.route_id;

        ELSE
          ----------------------------------------------------------------------
          -- Parent record does not exist in LOC_AHI_DATA.
          -- Mark as a new record and create new parent records.
          -- Message is logged after AHI specs, if exist, are transitoned.
          ----------------------------------------------------------------------
          ahi_data_action := ahi_create;
          comment_current := NULL;

          INSERT INTO loc_ahi_data (
            version,
            route_type,
            route_id,
            owning_facility,
            ahi_behavior )
          VALUES (
            p_Local_Version,
            p_Local_route_type,
            route.route_id,
            p_Facility_Id,
            'Adapted' );

        END IF;

        BEGIN
 
          INSERT INTO loc_ahi_trans (
            version,
            route_type,
            route_id,
            transition_id )
          VALUES (
            p_Local_Version,
            p_Local_route_type,
            route.route_id,
            l_transition_id );

        EXCEPTION
          ------------------------------------------------------
          -- In case LOC_AHI_TRANS record exsists, ignore error.
          ------------------------------------------------------
          WHEN DUP_VAL_ON_INDEX THEN
            NULL;  
        END;
        ------------------------------------------------------------------------
        -- Generate comment and update LOC_AHI_DATA comment
        ------------------------------------------------------------------------
        actrans_common.generate_comment(
             p_from_version  => p_ACES_version,
             p_from_source   => 'ACES',
             p_facility_id   => p_Facility_Id,
             p_loc_artcc     => p_Facility_id,
             p_comment_cur   => comment_current,
             p_comment_len   => comment_length,
             p_comment_new   => comment_new,
             p_data_key      => p_Local_route_type || ' ' || route.route_id,
             p_Err_Report    => p_Err_Report,
             p_Gen_Report    => p_Gen_Report,
             subprogram_name => Subprogram_Name,
             rc_comment      => rc_com );

        UPDATE loc_ahi_data SET comments = comment_new
         WHERE version         = p_Local_Version
           AND NVL(owning_facility, ' ')  = NVL(p_Facility_Id, ' ')
           AND route_type      = p_Local_route_type
           AND route_id        = route.route_ID;

        ------------------------------------------------------------------------
        -- Start a transaction here to create AHI specifiers for AHI route.
        -- If necessary, all inserts of AHI spec for route can be rolled back.
        ------------------------------------------------------------------------
        l_ahi_spec.DELETE;

        ------------------------------------------------------------------------
        -- Fetch all 6-keys of the route,
        -- following F-key (fix, ATC point) or A-key (airport)
        ------------------------------------------------------------------------
        FOR key_6 IN (
          SELECT DISTINCT fix_name loc, 'F' parent, seq_num
           FROM  aces_rte_fix_6_key
           WHERE version    = p_ACES_Version
             AND route_type = p_ACES_route_type
             AND route_id   = route.route_id
          UNION
          SELECT DISTINCT airport_id loc, 'A' parent, 9999 seq_num
           FROM  aces_rte_arr_6_key
           WHERE version    = p_ACES_Version
             AND route_type = p_ACES_route_type
             AND route_id   = route.route_id
          ORDER BY seq_num
        ) LOOP
          ----------------------------------------------------------------------
          -- Determine location (Fix, ATC point or airport).
          ----------------------------------------------------------------------
          verify_location(
               p_route_id => route.route_id,
               p_parent   => key_6.parent,
               p_loc_id   => key_6.loc,
               p_key      => '6',
               p_location => l_exp_loc,
               rc         => rc_com );
          l_exp_loc.seqn := key_6.seq_num;
          ----------------------------------------------------------------------
          -- For each 6-key, fetch all altitude ranges following it.
          -- l_exp_loc.alias is the ACES aiport ID or alias.
          ----------------------------------------------------------------------
          IF l_exp_loc.type = 'AIRPORT' THEN
            OPEN r_cur FOR
              SELECT lower_alt, upper_alt
               FROM  aces_rte_arr_6_key
               WHERE version    = p_ACES_Version
                 AND route_type = p_ACES_route_type
                 AND route_id   = route.route_id
                 AND airport_id = l_exp_loc.alias
              ORDER BY lower_alt, upper_alt;
          ELSE              -- FIX or ATC_POINT
            OPEN r_cur FOR
              SELECT lower_alt, upper_alt
               FROM  aces_rte_fix_6_key
               WHERE version    = p_ACES_Version
                 AND route_type = p_ACES_route_type
                 AND route_id   = route.route_id
                 AND fix_name   = l_exp_loc.name
                 AND seq_num    = key_6.seq_num
              ORDER BY lower_alt, upper_alt;
          END IF;

          LOOP
            --------------------------------------------------------------------
            -- Initiation (5-key) location altitude ranges, exited airspace
            -- list and entered airspace list are compiled for each
            -- 6-key altitude range.
            --------------------------------------------------------------------
            l_exit.DELETE;
            l_enter.DELETE;
            l_ini_lst.DELETE;

            FETCH r_cur INTO l_exp_loc.alt.lower, l_exp_upper_orig;
            EXIT WHEN r_cur%NOTFOUND;
            --------------------------------------------------------------------
            -- Original upper altitude (l_exp_upper_orig) will be used to
            -- fetch 5-key altitude ranges.
            --------------------------------------------------------------------
            l_exp_loc.alt.upper := norm_alt( l_exp_upper_orig );
            --------------------------------------------------------------------
            -- For each altitude range of the 6-key:
            --   - check for overlap with the last altitude range
            --   - fetch a list of airspaces (FAV/ARTCC) that together encompass
            --     it.
            --------------------------------------------------------------------
            IF loc_alt_range_overlap( l_exp_loc_prev, l_exp_loc ) THEN
              RAISE ahi_error;
            END IF;

            enter_count := get_airspaces(
                                p_location  => l_exp_loc,
                                p_airspace  => l_enter,
                                rc          => rc_com );

            IF ahi_trace THEN        -- debug
              print_airspace_list(
                   l_exp_loc.rte || ' ' || l_exp_loc.seqn || ' 6-key (' ||
                   l_exp_loc.type ||') '|| l_exp_loc.name || ' ' ||
                   l_exp_loc.alt.lower||'-'||l_exp_loc.alt.upper,
                   l_enter );
            END IF;
            --------------------------------------------------------------------
            -- Fetch all 5-keys preceding the the 6-key whose altitude ranges
            -- together encompass the 6-key altitude range.
            -- In order to eventually obtain a list of exited airspaces in
            -- ascending of alt ranges, the 5-key alt ranges must be in
            -- ascending order. The following logic inserts the current
            -- 5-key alt range -- retrieved in descending order of seq
            -- num -- into a list in ascending order of alt.
            -- Note that
            --   - if a 5-key has multiple altitude ranges, not all of them
            --     all of them may be selected, and overlapped altitude ranges
            --     are possible.
            --   - each 5-key alt range is not required to coincide with or
            --     encompassed by the 6-key alt range. It may partially overlap
            --     the 6-key alt range.
            -- 
            -- The transition algorithm searches backwards, in descending
            -- seq num, for 5-key alt ranges. 5-keys with alt ranges closet to
            -- thr 6-key has priority to be included as initiation locations.
            -- Distal 5-key ranges are included later to fill in gaps left
            -- unfilled by proximal 5-key ranges.
            --
            -- l_alt_lst(): A temporary list of altitude ranges is created to
            -- mirror the list of initiation locations ordered by altitude
            -- ranges. It is initialized with 2 records, FIRST and LAST as upper
            -- and lower limit of the list creating a gap to be filled by 5-key
            -- alt ranges.
            --------------------------------------------------------------------
            l_alt_lst.EXTEND(2);
            l_alt_lst(1).upper := l_exp_loc.alt.lower;    -- lower end of gap
            l_alt_lst(2).lower := l_exp_loc.alt.upper;    -- upper end of gap
            ------------------------------------------------------------
            -- Search for 5-keys assoc with a 6-key range may repeat a
            -- fetch of the same 5-key for a immediately preceding 6-key
            -- range. Clear previous 5-key name to avoid false overlap
            -- detection.
            ------------------------------------------------------------
            l_ini_loc_prev.name    := NULL;

            FOR key_5 IN (
              SELECT *
               FROM  aces_rte_fix_5_key
               WHERE version    = p_ACES_Version
                 AND route_type = p_ACES_route_type
                 AND route_id   = route.route_id
                 AND seq_num   <  key_6.seq_num
                 AND lower_alt < l_exp_upper_orig
                 AND upper_alt > l_exp_loc.alt.lower
              ORDER BY seq_num DESC, lower_alt ASC, upper_alt ASC
            ) LOOP
              ------------------------------------------------------------------
              -- For each 5-key altitude range,
              -- determine location type (Fix, ATC point or airport).
              ------------------------------------------------------------------
              verify_location(
                   p_route_id => route.route_id,
                   p_parent   => 'F',
                   p_loc_id   => key_5.fix_name,
                   p_key      => '5',
                   p_location => l_ini_loc,
                   rc         => rc_com );

              l_ini_loc.seqn      := key_5.seq_num;
              l_ini_loc.alt.lower := key_5.lower_alt;
              l_ini_loc.alt.upper := norm_alt( key_5.upper_alt );
              IF ahi_trace THEN
                trace_msg( '5-key: '||l_ini_loc.rte||' '||l_ini_loc.type||' '||
                           l_ini_loc.name||' '||
                           l_ini_loc.alt.lower||'-'||l_ini_loc.alt.upper );
              END IF;
              ------------------------------------------------------------------
              -- Check for overlap with the last altitude range of same location
              ------------------------------------------------------------------
              IF loc_alt_range_overlap( l_ini_loc_prev, l_ini_loc ) THEN
                RAISE ahi_error;
              END IF;

              ------------------------------------------------------------------
              -- Determine the position of initiation location in ascending
              -- order of altitude ranges. It is possible that the altitude
              -- range in l_ini_loc.alt is trimmed to fit into a gap.
              ------------------------------------------------------------------

              aclocal_util.Fill_Alt_Range_List (
                                p_alt_rec    => l_ini_loc.alt,
                                p_alt_lst    => l_alt_lst,
                                p_insert_pt  => i_insert,
                                p_gap_found  => l_gap_found );

              ----------------------------------------------------------
              -- The mirrored alt list array contains one more recrd.
              -- Decrement insertion point index.
              ----------------------------------------------------------
              i_insert := CASE WHEN i_insert < 1 THEN 0
                               ELSE i_insert - 1 END;
              ----------------------------------------------------------
              -- If insert point found, insert alt range
              ----------------------------------------------------------
              IF i_insert > 0 THEN
                l_ini_lst.EXTEND;
                ------------------------------------------------
                -- Push records starting from end of list until
                -- insertion point.
                ------------------------------------------------
                IF i_insert < l_ini_lst.LAST THEN
                   FOR i IN REVERSE i_insert..l_ini_lst.LAST-1 LOOP
                     l_ini_lst(i+1) := l_ini_lst(i);
                   END LOOP;
                END IF;
                l_ini_lst(i_insert) := l_ini_loc;
              END IF;
              ------------------------------------------------------------------
              -- 5-key ranges have covered the 6-key range without any gaps.
              -- Stop searching for more 5-key alt ranges.
              ------------------------------------------------------------------
              EXIT WHEN NOT l_gap_found;
            END LOOP;       -- For all 5-key altitude ranges
            --------------------------------------------------------------------
            -- Clear altitude list.
            --------------------------------------------------------------------
            l_alt_lst.DELETE;
            --------------------------------------------------------------------
            -- At this point, if the list is not null, we have a list of
            -- initiation loc (5-key) alt ranges in ascending alt order
            -- associated with one expiration loc (6-key) alt range.
            --------------------------------------------------------------------
            IF l_ini_lst.COUNT = 0 THEN
              ------------------------------------------------------------------
              -- No 5-key alt ranges associated with 6-key range, log warning.
              ------------------------------------------------------------------
              accom.log_error_msg(
                   put_file_handler     => p_Err_Report,
                   output_line          => 
                        p_ACES_route_type || ' ' || route.route_id ||' ' ||
                        location_info ( l_exp_loc ) || ', altitude range '||
                        to_char(l_exp_loc.alt.lower) || '-' ||
                        to_char(l_exp_loc.alt.upper) ||
                        ' has no associated initiation locations.',
                    logging_file_handler => p_Gen_Report,
                    calling_routine      => Subprogram_Name,
                    Error_Id             => 'TRANS0739',
                    Error_Code           => accom.g_rc_warning,
                    Rc                   => rc_com );
            ELSE
              IF l_gap_found THEN
                ----------------------------------------------------------------
                -- List has gap in 5-key alt ranges, log warning and continue
                -- to get airspaces.
                ----------------------------------------------------------------
                accom.log_error_msg(
                     put_file_handler     => p_Err_Report,
                     output_line          => 
                          p_ACES_route_type || ' ' || route.route_id ||' ' ||
                          location_info( l_exp_loc ) || ', altitude range ' ||
                          to_char(l_exp_loc.alt.lower) || '-' ||
                          to_char(l_exp_loc.alt.upper) ||
                         ' does not have associated initiation locations that'||
                          ' completely encompass the altitude range.',
                     logging_file_handler => p_Gen_Report,
                     calling_routine      => Subprogram_Name,
                     Error_Id             => 'TRANS0740',
                     Error_Code           => accom.g_rc_warning,
                     Rc                   => rc_com );

              END IF;
              ------------------------------------------------------------------
              -- Get airspaces for all 5-key alt ranges
              ------------------------------------------------------------------
              FOR i IN l_ini_lst.FIRST..l_ini_lst.LAST LOOP
                ----------------------------------------------------------------
                -- For each 5-key altitude range, fetch a list of FAVs that
                -- together encompass it.
                ----------------------------------------------------------------
                exit_count := 
                  exit_count + get_airspaces(
                                   p_location  => l_ini_lst(i),
                                   p_airspace  => l_exit,
                                   rc          => rc_com );

              END LOOP;     -- For each 5-key alt range of initiation location
            END IF;         -- Any 5-key alt ranges?
            IF ahi_trace THEN        -- debug
              print_airspace_list(
                   '5-key exited airspace list for 6-key (' ||
                    l_exp_loc.type ||') '|| l_exp_loc.name || ' ' ||
                    l_exp_loc.alt.lower||'-'||l_exp_loc.alt.upper,
                    l_exit );
            END IF;
            --------------------------------------------------------------------
            -- If both entered and exited airspaces exist,create AHI Specifiers.
            --------------------------------------------------------------------
            IF l_enter.COUNT > 0 AND l_exit.COUNT > 0 THEN
              ------------------------------------------------------------------
              -- At this point, we have one list each of airspaces
              --  - 1 6-key altitude range
              --  - 1 or more 5-key alt ranges that encompass the 6-key range
              ------------------------------------------------------------------
              exit_start := l_exit.FIRST;
              FOR en IN l_enter.FIRST..l_enter.LAST LOOP
                EXIT WHEN exit_start > l_exit.LAST;
                FOR ex IN exit_start..l_exit.LAST LOOP
                  --------------------------------------------------------------
                  -- Create record with the smallest possible altitude range.
                  -- Between exited and entered airspaces, take the lowest of
                  -- lower altitdues and the highest of upper altitudes.
                  --------------------------------------------------------------
                  IF l_exit(ex).alt.lower > l_enter(en).alt.lower THEN
                    l_alt.lower := l_exit(ex).alt.lower;
                  ELSE
                    l_alt.lower := l_enter(en).alt.lower;
                  END IF;
                  IF l_exit(ex).alt.upper < l_enter(en).alt.upper THEN
                    l_alt.upper := l_exit(ex).alt.upper;
                  ELSE
                   l_alt.upper := l_enter(en).alt.upper;
                  END IF;
                  IF ahi_trace THEN -- debug
                    trace_msg( 'Validate '||route.route_id||':'||
                         l_exit( ex).loc.name||'.'||l_exit( ex).name||'->'||
                         l_enter(en).loc.name||'.'||l_enter(en).name||':' ||
                         l_alt.lower||'-'||l_alt.upper );
                  END IF;
                  --------------------------------------------------------------
                  -- 1: Having NO overlap between alt ranges of the entered and
                  -- exited aisrspaces indicates that there is a gap in the list
                  -- of alt ranges. If this is a gap, do not create AHI spec.
                  -- 2: Call procedure to validate AHI spec. If it returns
                  -- FALSE, do not create AHI spec.
                  --------------------------------------------------------------
                  IF alt_range_overlap( l_exit(ex).alt, l_enter(en).alt ) THEN
                    IF validate_ahi_spec( p_entered => l_enter(en),
                                          p_exited  => l_exit(ex), 
                                          p_alt     => l_alt,
                                          p_rc      => rc_com ) THEN
                      --------------------------------------------------
                      -- Save data for subsequent sorting and creation
                      -- of LOC_AHI_SPEC record.
                      --------------------------------------------------
                      l_ahi_spec.EXTEND;
                      l_ahi_spec(l_ahi_spec.LAST).ent  := l_enter(en);
                      l_ahi_spec(l_ahi_spec.LAST).ext  := l_exit(ex);
                      l_ahi_spec(l_ahi_spec.LAST).alt  := l_alt;
                    END IF;
                  ELSIF l_alt.upper != l_alt.lower THEN
                    ----------------------------------------------------
                    -- Report a gap in airspace.
                    -- Note that in a gap, l_alt.upper < l_alt.lower and
                    -- if l_alt.upper = l_alt.lower, it is not a gap. 
                    ----------------------------------------------------
                    accom.log_error_msg(
                         put_file_handler     => p_Err_Report,
                         output_line          =>
                            p_ACES_route_type || ' ' || route.route_id ||', '||
                            location_info(l_enter(en).loc)||', '|| 
                            location_info(l_exit(ex).loc )||
                            ', a gap is found in the altitude range ' ||
                            to_char(l_alt.upper) || '-' ||
                            to_char(l_alt.lower) || '.',
                         logging_file_handler => p_Gen_Report,
                         calling_routine      => Subprogram_Name,
                         Error_Id             => 'TRANS0750',
                         Error_Code           => accom.g_rc_warning,
                         Rc                   => rc_com );
                  END IF;   -- Exited and Entered alt ranges overlap?
                  --------------------------------------------------------------
                  -- Compare exited with entered altitude ranges
                  --   (At least one of them contributes to upper alt)
                  -- A: Both cover upper altitude
                  --   Go to next entered AND exited airspace.
                  --   (Exit loop to step to next entered airspace and
                  --    increment to next exited airspace.)
                  -- B: Only entered airspace covers upper altitude.
                  --   Go to next entered airspace.
                  --   (Exit loop to step to next entered airspace.)
                  -- C: Only exited airspace covers upper altitude,
                  --   Go to next exited airspace.
                  --   (Continue loop to the next exited airspace.)
                  --------------------------------------------------------------
                  IF l_enter(en).alt.upper = l_alt.upper THEN
                    ----------------------------------------------------
                    -- Entered list steps up (exits inner loop)
                    -- Exited list also steps up if it also covers range
                    ----------------------------------------------------
                    exit_start := CASE WHEN l_exit(ex).alt.upper = l_alt.upper
                                       THEN ex + 1   -- case A
                                       ELSE ex       -- case B
                                  END;
                    EXIT;
                  END IF;
                  --------------------------------------------------------------
                  -- Case C: At this point l_exit(ex).alt.upper=l_alt.upper,
                  -- not l_enter(en).alt.upper.
                  -- Exited list steps up (stays in inner loop)
                  --------------------------------------------------------------
                END LOOP;   -- For all exited  airspaces
              END LOOP;     -- For all entered airspaces
            END IF;         -- If exited AND entered lists not empty
          END LOOP;         -- For all 6-key alt ranges of each expiration loc
          CLOSE r_cur;
        END LOOP;           -- For all 6-keys (expiration locations)

        IF l_ahi_spec.COUNT > 0 THEN
          ----------------------------------------------------------------------
          -- Sort AHI Specifiers for the route in order of
          -- - Seq Num of Expiration (6-key, entered) Loc in Route
          -- - Seq Num of Initiation (5-key, exited ) Loc associate with Exp Loc
          -- - Applicable altitude range lower altitude
          ----------------------------------------------------------------------
          swapped := TRUE;
          WHILE swapped AND l_ahi_spec.COUNT > 1 LOOP
            swapped := FALSE;
            FOR i IN 2..l_ahi_spec.LAST LOOP
              IF l_ahi_spec(i-1).ent.loc.seqn > l_ahi_spec(i).ent.loc.seqn OR

                (l_ahi_spec(i-1).ent.loc.seqn = l_ahi_spec(i).ent.loc.seqn AND
                 l_ahi_spec(i-1).ext.loc.seqn > l_ahi_spec(i).ext.loc.seqn ) OR

                (l_ahi_spec(i-1).ent.loc.seqn = l_ahi_spec(i).ent.loc.seqn AND 
                 l_ahi_spec(i-1).ext.loc.seqn = l_ahi_spec(i).ext.loc.seqn AND
                 l_ahi_spec(i-1).alt.lower    > l_ahi_spec(i).alt.lower ) THEN

                l_ahi_spec_tmp  := l_ahi_spec(i);
                l_ahi_spec(i)   := l_ahi_spec(i-1);
                l_ahi_spec(i-1) := l_ahi_spec_tmp;

                swapped := TRUE;
              END IF;
            END LOOP;
          END LOOP;
          ------------------------------------------------------
          -- Before insert, savepoint for rollback if necessary
          ------------------------------------------------------
          SAVEPOINT no_ahi_specs;
          ------------------------------------------------------
          -- Insert sorted AHI Specifiers for route
          ------------------------------------------------------
          FOR i IN l_ahi_spec.FIRST..l_ahi_spec.LAST LOOP
            BEGIN
              ----------------------------------
              -- Create LOC_AHI_SPEC record
              ----------------------------------
              INSERT INTO loc_ahi_spec (
                version,
                route_type,
                route_id,
                transition_id,
                applicable_lower_alt,
                applicable_upper_alt,
                seq_num,
                init_loc_is_atc_point,
                initiation_location,
                init_fix_id,
                init_icao_code,
                exited_airspace,
                expiration_location_type,
                expiration_location,
                exp_fix_id,
                exp_icao_code,
                ahi_target )
              VALUES (
                p_Local_Version,
                p_Local_route_type,
                route.route_id,
                l_transition_id,
                l_ahi_spec(i).alt.lower,
                l_ahi_spec(i).alt.upper,
                i*10,
                DECODE( l_ahi_spec(i).ext.loc.type, 'ATC_POINT', 'T', 'F' ),
                l_ahi_spec(i).ext.loc.name,
                l_ahi_spec(i).ext.loc.fix_id,
                l_ahi_spec(i).ext.loc.icao_code,
                l_ahi_spec(i).ext.name,
                l_ahi_spec(i).ent.loc.type,
                l_ahi_spec(i).ent.loc.name,
                l_ahi_spec(i).ent.loc.fix_id,
                l_ahi_spec(i).ent.loc.icao_code,
                l_ahi_spec(i).ent.name );

                p_Trans_Stat.Num_Spec_Added := p_Trans_Stat.Num_Spec_Added + 1;

                accom.log_error_msg(
                     put_file_handler     => p_Err_Report,
                     output_line          => 
                       'Created Auto Handoff Initiation Specifier for '||
                        ahi_spec_desc( p_route_id => route.route_id,
                                       p_enter    => l_ahi_spec(i).ent,
                                       p_exit     => l_ahi_spec(i).ext,
                                       p_alt      => l_ahi_spec(i).alt ) ||'.',
                     logging_file_handler => p_Gen_Report,
                     calling_routine      => Subprogram_Name,
                     Error_Id             => 'TRANS0734',
                     Error_Code           => accom.g_rc_no_error,
                     Rc                   => rc_com);

                if l_first_time = 0 then
                  l_first_time := 1;
                  for each_table in 
                    (select table_name from user_tables where table_name in 
                      ('LOC_AHI_DATA',
                       'LOC_AHI_SPEC',
                       'MRG_FIX_OVER_SPACE',
                       'LOC_ATC_OVER_SPACE',
                       'LOC_ARPT_OVER_SPACE',
                       'ACES_RTE_FIX_5_KEY',
                       'ACES_RTE_FIX_6_KEY',
                       'ACES_RTE_ARR_6_KEY')) 
                  loop
                    l_command_string := 'analyze table '||
                      each_table.table_name||' compute statistics';
                    execute immediate(l_command_string);
                    commit;
                  end loop;
                end if;

              EXCEPTION
                WHEN OTHERS THEN
                  accom.log_error_msg(
                      put_file_handler     => p_Err_Report,
                      output_line          =>
                         'Unexpected insert error '   ||
                         SQLCODE||' '||SQLERRM||': ' ||
                         ahi_spec_desc( p_route_id => route.route_id,
                                        p_enter    => l_ahi_spec(i).ent,
                                        p_exit     => l_ahi_spec(i).ext,
                                        p_alt      => l_ahi_spec(i).alt ) ||'.',
                      logging_file_handler => p_Gen_Report,
                      calling_routine      => Subprogram_Name,
                      Error_Id             => 'TRANS0000',
                      Error_Code           => accom.g_rc_error,
                      Rc                   => rc_com);
                RAISE ahi_error;
            END;            -- Insert one record into LOC_AHI_SPEC
          END LOOP;         -- Insert all records for route into LOC_AHI_SPEC

          ----------------------------------------------------------------------
          -- Now that all AHI specifiers of a route are added to the database,
          -- Call external procedure to validate each AHI specifier against
          -- other AHI specifiers of the same route.
          -- If the error requires that the AHI spec be discarded, delete it.
          ----------------------------------------------------------------------
          DECLARE
             l_count_rejected   PLS_INTEGER := 0;
          BEGIN
            FOR i IN l_ahi_spec.FIRST..l_ahi_spec.LAST LOOP

              l_count_rejected := 0;

              IF NOT validate_ahi_spec_post_insert (
                          p_ahi_spec      => l_ahi_spec(i),
                          p_num_rejected  => l_count_rejected,
                          p_rc            => rc_com ) THEN
                ------------------------------------------------
                -- Deduct deleted AHI specs from total.
                ------------------------------------------------
                p_Trans_Stat.Num_Spec_Added :=
                     p_Trans_Stat.Num_Spec_Added - l_count_rejected;
              END IF;
            END LOOP;
          EXCEPTION
            WHEN OTHERS THEN
              ----------------------------------
              -- Roll back all AHI spec inserts
              ----------------------------------
              ROLLBACK TO no_ahi_specs;

              accom.log_error_msg(
                  put_file_handler     => p_Err_Report,
                  output_line          => 'Unexpected AHI Transition Error '  ||
                                          'after AHI specifiers are inserted:'||
                                          SQLCODE || ' ' || SQLERRM ||
                                          '. Insert rolled back.', 
                 logging_file_handler => p_Gen_Report,
                 calling_routine      => Subprogram_Name,
                 Error_Id             => 'TRANS0000',
                 Error_Code           => accom.g_rc_error,
                 Rc                   => rc_com );

          END;              -- Block for validating inserted AHI specs
        END IF;             -- AHI Spec count > 0
      END IF;               -- route has 6-key
    EXCEPTION
      ------------------------------------------------------------------
      -- Exception block for processing an AHI Data and its AHI Specs
      ------------------------------------------------------------------
      WHEN ahi_error THEN
        --------------------------------------------------------
        -- Warning already logged. No AHI specs have been
        -- inserted. AHI route will be deleted.
        --------------------------------------------------------
        NULL;
      WHEN OTHERS THEN
          accom.log_error_msg(
              put_file_handler     => p_Err_Report,
              output_line          => 'Unexpected AHI Transition Error in ' ||
                                      'transitioning route:'||route.route_id||
                                      SQLCODE || ' ' || SQLERRM,
             logging_file_handler => p_Gen_Report,
             calling_routine      => Subprogram_Name,
             Error_Id             => 'TRANS0000',
             Error_Code           => accom.g_rc_error,
             Rc                   => rc_com );
    END;                    -- Block for processing of a route record

    ----------------------------------------------------------------------------
    -- Continue if more action required.
    ----------------------------------------------------------------------------
    IF ahi_data_action != ahi_none THEN
      --------------------------------------------------------------------------
      -- All validations completed. 
      -- Manange counts and log message for AHI route.
      -- At this point, ahi_data_action can be Create, Update or Delete.
      -- If ahi_data_action is Delete, the AHI spec count is non-zero.
      --------------------------------------------------------------------------
      SELECT count(*) INTO l_count_ahi_specs FROM loc_ahi_spec
       WHERE version         = p_Local_Version
         AND route_type      = p_Local_route_type
         AND route_id        = route.route_id;

      ------------------------------------------------------------------
      -- If no AHI specifiers left for AHI route, mark as discarded.
      ------------------------------------------------------------------
      ahi_data_action := CASE
                           WHEN l_count_ahi_specs <= 0 THEN ahi_discard
                           ELSE ahi_data_action END;

      IF ahi_data_action IN ( 'Deleted', 'Discarded' ) THEN
        --------------------------------------------------------
        -- Delete AHI route and children
        --------------------------------------------------------
        clean_ahi(
             p_Local_Version => p_Local_Version,
             p_Facility_Id   => p_Facility_Id,
             p_Route_Type    => p_Local_route_type,
             p_Route_id      => route.route_id,
             p_Err_Report    => p_Err_Report,
             p_Gen_Report    => p_Gen_Report,
             p_rc            => rc_com );
      END IF;

      CASE ahi_data_action
        WHEN ahi_create  THEN
          p_Trans_Stat.Num_Data_Added    := p_Trans_Stat.Num_Data_Added   + 1;
        WHEN ahi_update  THEN
          p_Trans_Stat.Num_Data_Updated  := p_Trans_Stat.Num_Data_Updated + 1;
        WHEN ahi_delete  THEN
          p_Trans_Stat.Num_Data_Deleted  := p_Trans_Stat.Num_Data_Deleted + 1;
        WHEN ahi_discard THEN
          p_Trans_Stat.Num_Data_Rejected := p_Trans_Stat.Num_Data_Rejected+ 1;
      END CASE;
      ------------------------------------------------------------------
      -- Log message based on action type
      ------------------------------------------------------------------
      accom.log_error_msg(
            put_file_handler     => p_Err_Report,
            output_line          => ahi_data_action ||
                 ' Auto Handoff Initiation Data record for '||
                 p_ACES_route_type || ' ' || route.route_id || '.',
            logging_file_handler => p_Gen_Report,
            calling_routine      => Subprogram_Name,
            Error_Id             => CASE ahi_data_action
                                      WHEN ahi_create  THEN 'TRANS0730'
                                      WHEN ahi_update  THEN 'TRANS0731'
                                      WHEN ahi_delete  THEN 'TRANS0732'
                                      WHEN ahi_discard THEN 'TRANS0733'
                                      ELSE                  'TRANS????'
                                    END,
            Error_Code           => CASE ahi_data_action
                                      WHEN ahi_discard THEN accom.g_rc_warning
                                      ELSE                  accom.g_rc_no_error
                                    END,
            Rc                   => rc_com  );

    END IF;                 -- ahi_data_action is NOT None
  END LOOP;                 -- For all ACES routes
EXCEPTION
  WHEN OTHERS THEN
    accom.log_error_msg(
         put_file_handler     => p_Err_Report,
         output_line          => 'ORA Error ' ||
                                 SQLCODE || ' ' || SQLERRM,
         logging_file_handler => p_Gen_Report,
         calling_routine      => Subprogram_Name,
         Error_Id             => 'TRANS0000',
         Error_Code           => accom.g_rc_error,
         Rc                   => rc_com);
    RAISE ahi_error;
END transition_ahi_pass_2;
--
--------------------------------------------------------------------------------
-- Procedure  : transition_ahi
-- Description: Common AHi Transition procedure for all route types
--              Pass 1: Verify locations and, if necessary, create ATC points
--              Pass 2: Transition AHI Data
--------------------------------------------------------------------------------
PROCEDURE transition_ahi(
        p_Facility_Id       IN     nat_facility_data.facility_id%TYPE,
        p_ACES_Version      IN     ctl_set_version.set_version%TYPE,
        p_Local_Version     IN     ctl_set_version.set_version%TYPE,
        p_route_type        IN     loc_val_route.route_type%TYPE,
        p_Clean_Trans_Ind   IN     ctl_transition_types.clean_trans_ind%TYPE,
        p_Process_Name      IN     loc_summary_process.process_name%TYPE,
        Report_Path         IN     accom.path_name%TYPE,
        Error_Name          IN     accom.path_name%TYPE,
        Report_File_Name    IN     accom.path_name%TYPE,
        rc_com              IN OUT accom.completion_code%TYPE ) IS

  Subprogram_Name     VARCHAR2(50) := 'actrans_ahi.transition_ahi';
  General_Error       UTL_FILE.FILE_TYPE;
  Transition_Error    UTL_FILE.FILE_TYPE;
  l_route_type        VARCHAR2(4) := 'A' || SUBSTR(p_route_type,2,3);
  Transition_Stat     Transition_Stat_Type;

BEGIN
  ----------------------------------------------------------------
  -- Open Report files
  ----------------------------------------------------------------
  General_Error := accom.File_Open(
       path_name            => Report_Path,
       file_name            => Report_File_Name,
       file_mode            => 'a',
       logging_file_handler => General_Error,
       calling_routine      => Subprogram_Name);

  Transition_Error := accom.File_Open(
       path_name            => Report_Path,
       file_name            => Error_Name,
       file_mode            => 'a',
       logging_file_handler => General_Error,
       calling_routine      => Subprogram_Name);

    ------------------------------------------------------------
    -- Pass 1:
    -- Verify that all locations are adapted.
    -- Create ATC points for boundary crossing fixes.
    ------------------------------------------------------------
    transition_ahi_pass_1 (
       p_Facility_Id     => p_Facility_Id,
       p_ACES_Version    => p_ACES_Version,
       p_Local_Version   => p_Local_Version,
       p_ACES_route_type => p_route_type,
       p_Local_route_type=> l_route_type,
       p_Clean_Trans_Ind => p_Clean_Trans_Ind,
       p_Err_Report      => Transition_Error,
       p_Gen_Report      => General_Error,
       p_Trans_Stat      => Transition_Stat,
       rc_com            => rc_com );

    IF rc_com >= accom.g_rc_error THEN
      ------------------------------------------------------------
      -- Error in pass 1
      -- Store summary data
      ------------------------------------------------------------
      acsummary.store_summary_detail(
       p_operation     => Acsummary.Transition,
       p_version       => p_Local_Version,
       p_process_name  => p_Process_Name,
       p_detail        => 'ATC points added',
       p_process_total => Transition_Stat.Num_ATC_Added );

    ELSE
      ------------------------------------------------------------
      -- No error in pass 1 conti nue to pass 2.
      -- Transition 5/6-key to LOC AHI Data
      ------------------------------------------------------------
      transition_ahi_pass_2 (
         p_Facility_Id     => p_Facility_Id,
         p_ACES_Version    => p_ACES_Version,
         p_Local_Version   => p_Local_Version,
         p_ACES_route_type => p_route_type,
         p_Local_route_type=> l_route_type,
         p_Clean_Trans_Ind => p_Clean_Trans_Ind,
         p_Err_Report      => Transition_Error,
         p_Gen_Report      => General_Error,
         p_Trans_Stat      => Transition_Stat,
         rc_com            => rc_com );

      ----------------------------------------------------------------
      -- Store transition summary data
      ----------------------------------------------------------------
      acsummary.store_summary_detail(
           p_operation     => Acsummary.Transition,
           p_version       => p_Local_Version,
           p_process_name  => p_Process_Name,
           p_detail        => l_route_type||' AHI routes updated',
           p_process_total => Transition_Stat.Num_Data_Updated );

      acsummary.store_summary_detail(
           p_operation     => Acsummary.Transition,
           p_version       => p_Local_Version,
           p_process_name  => p_Process_Name,
           p_detail        => l_route_type||' AHI routes added',
           p_process_total => Transition_Stat.Num_Data_Added );

      acsummary.store_summary_detail(
           p_operation     => Acsummary.Transition,
           p_version       => p_Local_Version,
           p_process_name  => p_Process_Name,
           p_detail        => l_route_type||' AHI routes deleted',
           p_process_total => Transition_Stat.Num_Data_Deleted );

      acsummary.store_summary_detail(
           p_operation     => Acsummary.Transition,
           p_version       => p_Local_Version,
           p_process_name  => p_Process_Name,
           p_detail        => l_route_type||' AHI routes discarded',
           p_process_total => Transition_Stat.Num_Data_Rejected );

      acsummary.store_summary_detail(
           p_operation     => Acsummary.Transition,
           p_version       => p_Local_Version,
           p_process_name  => p_Process_Name,
           p_detail        => l_route_type||' AHI specifiers added',
           p_process_total => Transition_Stat.Num_Spec_Added );
    END IF;

  --------------------------------------------------------------
  -- Close report files
  --------------------------------------------------------------
  Accom.File_Close (
       close_file_handler   => Transition_Error,
       logging_file_handler => General_Error,
       calling_routine      => Subprogram_Name);

  Accom.File_Close(
       close_file_handler   => General_Error,
       logging_file_handler => General_Error,
       calling_routine      => Subprogram_Name);

EXCEPTION
  WHEN ahi_error THEN
    --------------------------------------------
    -- Error message has already been logged.
    --------------------------------------------
    rc_com := accom.g_rc_error;
    --------------------------------------------------------------
    -- Close report files
    --------------------------------------------------------------
    Accom.File_Close (
         close_file_handler   => Transition_Error,
         logging_file_handler => General_Error,
         calling_routine      => Subprogram_Name);

    Accom.File_Close(
         close_file_handler   => General_Error,
         logging_file_handler => General_Error,
         calling_routine      => Subprogram_Name);

  WHEN OTHERS THEN

    accom.log_error_msg(
         put_file_handler     => Transition_Error,
         output_line          => 'ORA Error ' ||
                                 SQLCODE || ' ' || SQLERRM,
         logging_file_handler => General_Error,
         calling_routine      => Subprogram_Name,
         Error_Id             => 'TRANS0000',
         Error_Code           => accom.g_rc_error,
         Rc                   => rc_com);

    --------------------------------------------------------------
    -- Close report files
    --------------------------------------------------------------
    Accom.File_Close (
         close_file_handler   => Transition_Error,
         logging_file_handler => General_Error,
         calling_routine      => Subprogram_Name);

    Accom.File_Close(
         close_file_handler   => General_Error,
         logging_file_handler => General_Error,
         calling_routine      => Subprogram_Name);

END transition_ahi;
--
--------------------------------------------------------------------------------
-- Procedure  : transition_par
-- Description: See Package Spec
--------------------------------------------------------------------------------
  PROCEDURE transition_par_ahi (
       p_Facility_Id       IN     nat_facility_data.facility_id%TYPE,
       p_ACES_Version      IN     ctl_set_version.set_version%TYPE,
       p_Local_Version     IN     ctl_set_version.set_version%TYPE,
       p_Clean_Trans_Ind   IN     ctl_transition_types.clean_trans_ind%TYPE,
       p_Process_Name      IN     loc_summary_process.process_name%TYPE,
       Report_Path         IN     accom.path_name%TYPE,
       Error_Name          IN     accom.path_name%TYPE,
       Report_File_Name    IN     accom.path_name%TYPE,
       rc_ahi                 OUT accom.completion_code%TYPE ) IS
  BEGIN
    rc_ahi := accom.g_rc_no_error;
    ------------------------------------------------------------
    -- Call common procedure transition_ahi
    ------------------------------------------------------------
    transition_ahi (
       p_Facility_Id     => p_Facility_Id,
       p_ACES_Version    => p_ACES_Version,
       p_Local_Version   => p_Local_Version,
       p_route_type      => 'PAR',
       p_Clean_Trans_Ind => p_Clean_Trans_Ind,
       p_Process_Name    => p_Process_Name,
       Report_Path       => Report_Path,
       Report_File_Name  => Report_File_Name,
       Error_Name        => Error_Name,
       rc_com            => rc_ahi );
  END transition_par_ahi;
--
--------------------------------------------------------------------------------
-- Procedure  : transition_pdar_ahi
-- Description: See Package Spec
--------------------------------------------------------------------------------
  PROCEDURE transition_pdar_ahi (
       p_Facility_Id       IN     nat_facility_data.facility_id%TYPE,
       p_ACES_Version      IN     ctl_set_version.set_version%TYPE,
       p_Local_Version     IN     ctl_set_version.set_version%TYPE,
       p_Clean_Trans_Ind   IN     ctl_transition_types.clean_trans_ind%TYPE,
       p_Process_Name      IN     loc_summary_process.process_name%TYPE,
       Report_Path         IN     accom.path_name%TYPE,
       Error_Name          IN     accom.path_name%TYPE,
       Report_File_Name    IN     accom.path_name%TYPE,
       rc_ahi                 OUT accom.completion_code%TYPE ) IS
  BEGIN
    rc_ahi := accom.g_rc_no_error;
    ------------------------------------------------------------
    -- Call common procedure transition_ahi
    ------------------------------------------------------------
    transition_ahi (
       p_Facility_Id     => p_Facility_Id,
       p_ACES_Version    => p_ACES_Version,
       p_Local_Version   => p_Local_Version,
       p_route_type      => 'PDAR',
       p_Clean_Trans_Ind => p_Clean_Trans_Ind,
       p_Process_Name    => p_Process_Name,
       Report_Path       => Report_Path,
       Report_File_Name  => Report_File_Name,
       Error_Name        => Error_Name,
       rc_com            => rc_ahi );
  END transition_pdar_ahi;
--
--------------------------------------------------------------------------------
-- Procedure  : transition_pdr_ahi
-- Description: See Package Spec
--------------------------------------------------------------------------------
  PROCEDURE transition_pdr_ahi (
       p_Facility_Id       IN     nat_facility_data.facility_id%TYPE,
       p_ACES_Version      IN     ctl_set_version.set_version%TYPE,
       p_Local_Version     IN     ctl_set_version.set_version%TYPE,
       p_Clean_Trans_Ind   IN     ctl_transition_types.clean_trans_ind%TYPE,
       p_Process_Name      IN     loc_summary_process.process_name%TYPE,
       Report_Path         IN     accom.path_name%TYPE,
       Error_Name          IN     accom.path_name%TYPE,
       Report_File_Name    IN     accom.path_name%TYPE,
       rc_ahi                 OUT accom.completion_code%TYPE ) IS
  BEGIN
    rc_ahi := accom.g_rc_no_error;
    ------------------------------------------------------------
    -- Call common procedure transition_ahi
    ------------------------------------------------------------
    transition_ahi (
       p_Facility_Id     => p_Facility_Id,
       p_ACES_Version    => p_ACES_Version,
       p_Local_Version   => p_Local_Version,
       p_route_type      => 'PDR',
       p_Clean_Trans_Ind => p_Clean_Trans_Ind,
       p_Process_Name    => p_Process_Name,
       Report_Path       => Report_Path,
       Report_File_Name  => Report_File_Name,
       Error_Name        => Error_Name,
       rc_com            => rc_ahi );
  END transition_pdr_ahi;
--
END actrans_ahi;
/
