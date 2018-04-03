CREATE OR REPLACE PACKAGE actrans_ahi AS
--------------------------------------------------------------------------------
-- Component Name:       Transition
-- Number:               actrans_ahi.pkgs
-- Applicable Segments:  EADP LAADP
-- CAS Product Used:     Oracle Relational Database, PL/SQL
--
-- Description:
--    This package contains the code objects required to transition various
--    ACES data to the corresponding EADP Auto Handoff Initiation (AHI) tables.
--
-- Methods:
--    Procedure transition_par_ahi
--    Procedure transition_pdar_ahi
--    Procedure transition_pdr_ahi
--
-- Change History:
--
-- CL  Reason    Description of Change                          Pgmr  MM/DD/YYYY
-- --  --------- --------------------------------------------   ----  ----------
--  1  ER_873    Module creation                                CCF   03/03/2011
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- Procedure  : transition_par_ahi
-- Description: This routine takes the ACES PAR data from the
--              relevant ACES_% tables and inserts it in the appropriate local
--              AHI tables
--              This procedure will call a common procedure (non-visible)
--              to perform transition functions common to all AHI transitions.
--              Note that gaps and overlaps in altitude ranges will be logged
--              at cross-validation and not during transition.
--
-- Inputs     :
--   p_Facility_Id       - facility the data is being transitioned for
--   p_ACES_Version      - version of ACES to transition from
--   p_Local_Version     - version of Local data to transition ACES data to
--   p_Clean_Trans_Ind   - clean transition indicator (T/F)
--   p_Process_Name      - Process name used to log summary details
--   Report_Path         - Path to the Reports directory
--   Error_Name          - name of transition error report
--   Report_File_Name    - name of general report file
--
-- Outputs    :
--   rc_ahi              - indicating result of processing.
--
-- Exceptions:
-- OTHERS
--------------------------------------------------------------------------------
PROCEDURE  transition_par_ahi(
     p_Facility_Id       IN     nat_facility_data.facility_id%TYPE,
     p_ACES_Version      IN     ctl_set_version.set_version%TYPE,
     p_Local_Version     IN     ctl_set_version.set_version%TYPE,
     p_Clean_Trans_Ind   IN     ctl_transition_types.clean_trans_ind%TYPE,
     p_Process_Name      IN     loc_summary_process.process_name%TYPE,
     Report_Path         IN     accom.path_name%TYPE,
     Error_Name          IN     accom.path_name%TYPE,
     Report_File_Name    IN     accom.path_name%TYPE,
     rc_ahi                 OUT accom.completion_code%TYPE );

--------------------------------------------------------------------------------
-- Procedure  : transition_pdar_ahi
-- Description: This routine takes the ACES PDAR data from the
--              relevant ACES_% tables and inserts it in the appropriate local
--              AHI tables
--              This procedure will call a common procedure (non-visible)
--              to perform transition functions common to all AHI transitions.
--              Note that gaps and overlaps in altitude ranges will be logged
--              at cross-validation and not during transition.
--
-- Inputs     :
--   p_Facility_Id       - facility the data is being transitioned for
--   p_ACES_Version      - version of ACES to transition from
--   p_Local_Version     - version of Local data to transition ACES data to
--   p_Clean_Trans_Ind   - clean transition indicator (T/F)
--   p_Process_Name      - Process name used to log summary details
--   Report_Path         - Path to the Reports directory
--   Error_Name          - name of transition error report
--   Report_File_Name    - name of general report file
--
-- Outputs    :
--   rc_ahi              - indicating result of processing.
--
-- Exceptions:
-- OTHERS
--------------------------------------------------------------------------------
PROCEDURE  transition_pdar_ahi(
     p_Facility_Id       IN     nat_facility_data.facility_id%TYPE,
     p_ACES_Version      IN     ctl_set_version.set_version%TYPE,
     p_Local_Version     IN     ctl_set_version.set_version%TYPE,
     p_Clean_Trans_Ind   IN     ctl_transition_types.clean_trans_ind%TYPE,
     p_Process_Name      IN     loc_summary_process.process_name%TYPE,
     Report_Path         IN     accom.path_name%TYPE,
     Error_Name          IN     accom.path_name%TYPE,
     Report_File_Name    IN     accom.path_name%TYPE,
     rc_ahi                 OUT accom.completion_code%TYPE );
--
--------------------------------------------------------------------------------
-- Procedure  : transition_pdr
-- Description: This routine takes the ACES PDR data from the
--              relevant ACES_% tables and inserts it in the appropriate local
--              AHI tables
--              This procedure will call a common procedure (non-visible)
--              to perform transition functions common to all AHI transitions.
--              Note that gaps and overlaps in altitude ranges will be logged
--              at cross-validation and not during transition.
--
-- Inputs     :
--   p_Facility_Id       - facility the data is being transitioned for
--   p_ACES_Version      - version of ACES to transition from
--   p_Local_Version     - version of Local data to transition ACES data to
--   p_Clean_Trans_Ind   - clean transition indicator (T/F)
--   p_Process_Name      - Process name used to log summary details
--   Report_Path         - Path to the Reports directory
--   Error_Name          - name of transition error report
--   Report_File_Name    - name of general report file
--
-- Outputs    :
--   rc_ahi              - indicating result of processing.
--
-- Exceptions:
-- OTHERS
--------------------------------------------------------------------------------
PROCEDURE  transition_pdr_ahi(
     p_Facility_Id       IN     nat_facility_data.facility_id%TYPE,
     p_ACES_Version      IN     ctl_set_version.set_version%TYPE,
     p_Local_Version     IN     ctl_set_version.set_version%TYPE,
     p_Clean_Trans_Ind   IN     ctl_transition_types.clean_trans_ind%TYPE,
     p_Process_Name      IN     loc_summary_process.process_name%TYPE,
     Report_Path         IN     accom.path_name%TYPE,
     Error_Name          IN     accom.path_name%TYPE,
     Report_File_Name    IN     accom.path_name%TYPE,
     rc_ahi                 OUT accom.completion_code%TYPE );

END actrans_ahi;
/
