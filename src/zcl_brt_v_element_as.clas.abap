CLASS zcl_brt_v_element_as DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    CLASS-DATA  gt_status TYPE TABLE OF zdd_status_vh_as.
    CLASS-DATA gt_priority TYPE TABLE OF zdd_priority_vh_as.
    CLASS-METHODS class_constructor.
    INTERFACES if_sadl_exit_calc_element_read.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_brt_v_element_as IMPLEMENTATION.
  METHOD if_sadl_exit_calc_element_read~calculate.

    DATA lt_original_data TYPE TABLE OF zc_dt_inct_as WITH DEFAULT KEY.

    lt_original_data = CORRESPONDING #( it_original_data ).
    LOOP AT it_requested_calc_elements ASSIGNING FIELD-SYMBOL(<fs_requested>).
      IF <fs_requested> = 'STATUSDESCRIPTION'.
        LOOP AT lt_original_data ASSIGNING FIELD-SYMBOL(<fs_original_data>).
          <fs_original_data>-StatusDescription = gt_status[ StatusCode = <fs_original_data>-Status ]-StatusDescription.
        ENDLOOP.
      ENDIF.
      IF <fs_requested> = 'PRIORITYDESCRIPTION'.
        LOOP AT lt_original_data ASSIGNING FIELD-SYMBOL(<fs_original_priority_data>).
        IF <fs_original_priority_data>-Priority IS NOT INITIAL.
          <fs_original_priority_data>-PriorityDescription = gt_priority[ PriorityCode = <fs_original_priority_data>-Priority ]-PriorityDescription.
          ENDIF.
        ENDLOOP.
      ENDIF.
      ENDLOOP.
      ct_calculated_data = CORRESPONDING #( lt_original_data ).
    ENDMETHOD.

    METHOD if_sadl_exit_calc_element_read~get_calculation_info.
      CASE iv_entity.
        WHEN 'zc_dt_inct_as'.
          LOOP AT it_requested_calc_elements INTO DATA(ls_requested_calc_elem).
            IF ls_requested_calc_elem EQ 'status'.
              INSERT CONV #( 'STATUSDESCRIPTION' ) INTO TABLE et_requested_orig_elements.
            ENDIF.
            IF ls_requested_calc_elem EQ 'priority'.
              INSERT CONV #( 'PRIORITYDESCRIPTION' ) INTO TABLE et_requested_orig_elements.
            ENDIF.
          ENDLOOP.
      ENDCASE.
    ENDMETHOD.

    METHOD class_constructor.

      SELECT * FROM zdd_status_vh_as
       INTO TABLE @gt_status.


       SELECT * FROM zdd_priority_vh_as
       INTO TABLE @gt_priority.
      ENDMETHOD.

ENDCLASS.
