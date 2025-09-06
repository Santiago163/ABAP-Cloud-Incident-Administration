CLASS lhc_Incident DEFINITION INHERITING FROM cl_abap_behavior_handler.
  PUBLIC SECTION.
    CONSTANTS: BEGIN OF gs_status,
                 open        TYPE zde_status_as VALUE 'OP',
                 in_progress TYPE zde_status_as VALUE 'IP',
                 pending     TYPE zde_status_as VALUE 'PE',
                 completed   TYPE zde_status_as VALUE 'CO',
                 closed      TYPE zde_status_as VALUE 'CL',
                 canceled    TYPE zde_status_as VALUE 'CN',
               END OF gs_status.

  PRIVATE SECTION.


    METHODS get_instance_authorizations FOR INSTANCE AUTHORIZATION
      IMPORTING keys REQUEST requested_authorizations FOR Incident RESULT result.

    METHODS get_global_authorizations FOR GLOBAL AUTHORIZATION
      IMPORTING REQUEST requested_authorizations FOR Incident RESULT result.
    METHODS get_instance_features FOR INSTANCE FEATURES
      IMPORTING keys REQUEST requested_features FOR Incident RESULT result.
    METHODS changestatus FOR MODIFY
      IMPORTING keys FOR ACTION incident~changestatus RESULT result.
    METHODS setdefaultvalues FOR DETERMINE ON MODIFY
      IMPORTING keys FOR incident~setdefaultvalues.
    METHODS sethistory FOR MODIFY
      IMPORTING keys FOR ACTION incident~sethistory.

    METHODS setdefaulthistory FOR DETERMINE ON SAVE
      IMPORTING keys FOR incident~setdefaulthistory.
    METHODS get_history_index
      EXPORTING
        ev_incuuid      TYPE sysuuid_x16
      RETURNING
        value(r_result) TYPE zde_his_id_lgl.

ENDCLASS.

CLASS lhc_Incident IMPLEMENTATION.

  METHOD get_instance_authorizations.
  ENDMETHOD.

  METHOD get_global_authorizations.
  ENDMETHOD.

  METHOD get_instance_features.
      DATA lv_history_index TYPE zde_his_id_lgl.
    READ ENTITIES OF zr_dt_inct_as IN LOCAL MODE
       ENTITY Incident
         FIELDS ( Status )
         WITH CORRESPONDING #( keys )
       RESULT DATA(incidents)
       FAILED failed.

** Disable changeStatus for Incidents Creation
    DATA(lv_create_action) = lines( incidents ).
    IF lv_create_action EQ 1.
      lv_history_index = get_history_index( IMPORTING ev_incuuid = incidents[ 1 ]-IncUUID ).
    ELSE.
      lv_history_index = 1.
    ENDIF.

    result = VALUE #( FOR incident IN incidents
                          ( %tky                   = incident-%tky
                            %action-ChangeStatus   = COND #( WHEN incident-Status = gs_status-completed OR
                                                                  incident-Status = gs_status-closed    OR
                                                                  incident-Status = gs_status-canceled  OR
                                                                  lv_history_index = 0
                                                             THEN if_abap_behv=>fc-o-disabled
                                                             ELSE if_abap_behv=>fc-o-enabled )

                            %assoc-_History       = COND #( WHEN incident-Status = gs_status-completed OR
                                                                 incident-Status = gs_status-closed    OR
                                                                 incident-Status = gs_status-canceled  OR
                                                                 lv_history_index = 0
                                                            THEN if_abap_behv=>fc-o-disabled
                                                            ELSE if_abap_behv=>fc-o-enabled )
                          ) ).
  ENDMETHOD.

  METHOD changeStatus.
  ENDMETHOD.

  METHOD setDefaultValues.
** Read root entity entries
    READ ENTITIES OF zr_dt_inct_as IN LOCAL MODE
     ENTITY Incident
     FIELDS ( CreationDate
              Status ) WITH CORRESPONDING #( keys )
     RESULT DATA(incidents).

** This important for logic
    DELETE incidents WHERE CreationDate IS NOT INITIAL.

    CHECK incidents IS NOT INITIAL.

** Get Last index from Incidents

    SELECT MAX( incident_id )  FROM zdt_inct_as WHERE incident_id IS NOT NULL INTO @DATA(lv_max).

    DATA(lv_max_inct_id) = COND int8(
      WHEN lv_max IS NOT INITIAL THEN lv_max + 1
      ELSE 1 ).


** Modify status in Root Entity
    MODIFY ENTITIES OF zr_dt_inct_as IN LOCAL MODE
      ENTITY Incident
      UPDATE
      FIELDS ( IncidentID
               CreationDate
               Status )
      WITH VALUE #(  FOR incident IN incidents ( %tky = incident-%tky
                                                 IncidentID = lv_max_inct_id
                                                 CreationDate = cl_abap_context_info=>get_system_date( )
                                                 Status       = gs_status-open )  ).
  ENDMETHOD.

  METHOD setHistory.
** Declaration of necessary variables
    DATA: lt_updated_root_entity TYPE TABLE FOR UPDATE zr_dt_inct_as,
          lt_association_entity  TYPE TABLE FOR CREATE zr_dt_inct_as\_History,
          lv_exception           TYPE string,
          ls_incident_history    TYPE zdt_inct_h_as.


** Iterate through the keys records to get parameters for validations
    READ ENTITIES OF zr_dt_inct_as IN LOCAL MODE
         ENTITY Incident
         ALL FIELDS WITH CORRESPONDING #( keys )
         RESULT DATA(incidents).

** Get parameters
    LOOP AT incidents ASSIGNING FIELD-SYMBOL(<incident>).
** Get Last index from Incidents

      SELECT MAX( his_id )  FROM zdt_inct_h_as WHERE inc_uuid EQ @<incident>-IncUUID AND
              his_uuid IS NOT NULL INTO @DATA(lv_max).

      ls_incident_history-his_id = COND int8(
        WHEN lv_max IS NOT INITIAL THEN lv_max + 1
        ELSE 1 ).


      TRY.
          ls_incident_history-inc_uuid = cl_system_uuid=>create_uuid_x16_static( ).
        CATCH cx_uuid_error INTO DATA(lo_error).
          lv_exception = lo_error->get_text(  ).
      ENDTRY.

      APPEND VALUE #( %tky = <incident>-%tky
                      %target = VALUE #( (  HisUUID = ls_incident_history-inc_uuid
                                            IncUUID = <incident>-IncUUID
                                            HisID = ls_incident_history-his_id
                                            NewStatus = <incident>-Status
                                            Text = 'First Incident' ) )
                                             ) TO lt_association_entity.
    ENDLOOP.
    UNASSIGN <incident>.

    FREE incidents. " Free entries in incidents

    MODIFY ENTITIES OF zr_dt_inct_as IN LOCAL MODE
     ENTITY Incident
     CREATE BY \_History FIELDS ( HisUUID
                                  IncUUID
                                  HisID
                                  PreviousStatus
                                  NewStatus
                                  Text )
        AUTO FILL CID
        WITH lt_association_entity.

  ENDMETHOD.

  METHOD setDefaultHistory.

** Execute internal action to UPDATE Flight Date
    MODIFY ENTITIES OF zr_dt_inct_as IN LOCAL MODE
    ENTITY Incident
    EXECUTE setHistory
       FROM CORRESPONDING #( keys ).
  ENDMETHOD.


  METHOD get_history_index.

  ENDMETHOD.

ENDCLASS.
