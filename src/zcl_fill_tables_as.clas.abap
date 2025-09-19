CLASS zcl_fill_tables_as DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_oo_adt_classrun .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_fill_tables_as IMPLEMENTATION.


  METHOD if_oo_adt_classrun~main.


    DELETE FROM zdt_inct_h_as WHERE 1 = 1.

    DELETE FROM zdt_d_inct_h_as WHERE 1 = 1.


    DELETE FROM zdt_inct_as WHERE 1 = 1.

    DELETE FROM zdt_d_inct_as where 1 = 1.

    SELECT
    language,
    value_low,
    text
    FROM ddcds_customer_domain_value_t( p_domain_name = 'ZDO_STATUS_AS' )
    INTO TABLE @DATA(lt_status).

    SELECT @sy-mandt AS client, value_low AS status_code, text AS status_description
    FROM @lt_status AS x
    WHERE language = @sy-langu
    INTO TABLE @DATA(lt_filter_status).

    MODIFY  zdt_status_as FROM TABLE @lt_filter_status.

    SELECT
    language,
    value_low,
    text
    FROM ddcds_customer_domain_value_t( p_domain_name = 'ZDO_PRIORITY_AS' )
    INTO TABLE @DATA(lt_priority).

    SELECT @sy-mandt AS client, value_low AS priority_code, text AS priority_description
    FROM @lt_priority AS x
    WHERE language = @sy-langu
    INTO TABLE @DATA(lt_filter_priority).

    MODIFY zdt_priority_as FROM TABLE @lt_filter_priority.

  ENDMETHOD.
ENDCLASS.
