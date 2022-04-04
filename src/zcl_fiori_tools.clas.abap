class ZCL_FIORI_TOOLS definition
  public
  final
  create public .

public section.

  methods CONSTRUCTOR
    importing
      !IO_CONTEXT_E type ref to /IWBEP/IF_MGW_REQ_ENTITYSET optional
      !IO_CONTEXT_C type ref to /IWBEP/IF_MGW_REQ_ENTITY_C optional
      !IO_CONTEXT_D type ref to /IWBEP/IF_MGW_REQ_ENTITY_D optional
      !IO_CONTEXT_U type ref to /IWBEP/IF_MGW_REQ_ENTITY_U optional
      !IO_CONTEXT_R type ref to /IWBEP/IF_MGW_REQ_ENTITY optional .
  methods GET_FILTER
    importing
      !IB_CONVERSION type FLAG optional
      value(IV_FIELD) type FIELDNAME
    exporting
      value(ER_RANGE) type ANY
      value(EV_VALUE) type ANY .
  methods GET_KEY
    importing
      value(IV_FIELD) type FIELDNAME
      value(IB_CONVERSION) type FLAG optional
    exporting
      value(EV_VALUE) type ANY .
  methods GET_TOP
    returning
      value(RV_TOP) type STRING .
  methods GET_SKIP
    returning
      value(RV_TOP) type STRING .
  methods CONVERSION_INPUT
    importing
      !IV_VALUE type ANY
      !IV_FIELD type FIELDNAME
    exporting
      value(EV_VALUE) type ANY .
  methods REPLACE_TURKISH_CHARACTERS
    changing
      value(CV_TEXT) type CLIKE .
  methods RAISE_EXCEPTION
    importing
      value(IV_TEXT1) type SYMSGV optional
      value(IV_TEXT2) type SYMSGV optional
      value(IV_TEXT3) type SYMSGV optional
      value(IV_TEXT4) type SYMSGV optional
    raising
      /IWBEP/CX_MGW_BUSI_EXCEPTION
      /IWBEP/CX_MGW_TECH_EXCEPTION .
  methods HANDLE_SORTING
    exporting
      !EV_ORDERBY type STRING .
protected section.
private section.

  data CONTEXT_E type ref to /IWBEP/IF_MGW_REQ_ENTITYSET .
  data CONTEXT_C type ref to /IWBEP/IF_MGW_REQ_ENTITY_C .
  data CONTEXT_D type ref to /IWBEP/IF_MGW_REQ_ENTITY_D .
  data CONTEXT_U type ref to /IWBEP/IF_MGW_REQ_ENTITY_U .
  data CONTEXT_R type ref to /IWBEP/IF_MGW_REQ_ENTITY .
ENDCLASS.



CLASS ZCL_FIORI_TOOLS IMPLEMENTATION.


  METHOD constructor.

    context_e = io_context_e.
    context_r = io_context_r.
    context_c = io_context_c.
    context_u = io_context_u.
    context_d = io_context_d.

  ENDMETHOD.


  METHOD conversion_input.

    DATA: ls_dd01l TYPE dd01l,
          ls_dd03l TYPE dd03l,
          ls_dd04l TYPE dd04l.

    DATA: lv_exit_name TYPE string,
          lr_data      TYPE REF TO data,
          lx_root      TYPE REF TO cx_root.

    FIELD-SYMBOLS: <value> TYPE any.

    CLEAR: ls_dd01l, ls_dd03l, ls_dd04l, ev_value,
           lv_exit_name, lr_data.

**********************************************************************
* 1. get data-element of the field
**********************************************************************
    SELECT SINGLE *
      FROM dd03l
      INTO ls_dd03l
*     WHERE tabname EQ iv_table
       WHERE fieldname EQ iv_field.
    IF sy-subrc = 0.

**********************************************************************
* 2. get the rollname of the data-element
**********************************************************************
      SELECT SINGLE *
        FROM dd04l
        INTO ls_dd04l
       WHERE rollname = ls_dd03l-rollname.

      IF sy-subrc = 0.

**********************************************************************
* 3. get the convert routine
**********************************************************************
        SELECT SINGLE *
          FROM dd01l
          INTO ls_dd01l
         WHERE domname = ls_dd04l-domname.
        IF sy-subrc = 0 AND ls_dd01l-convexit IS NOT INITIAL.

**********************************************************************
* 4. apply convert routine
**********************************************************************

          " create corresponding field
          TRY.
              CREATE DATA lr_data TYPE (ls_dd03l-rollname).
            CATCH cx_sy_create_data_error INTO lx_root.
              EXIT.
          ENDTRY.

          " dereferencing
          ASSIGN lr_data->* TO <value>.
          IF sy-subrc = 0.

            " apply convert routine
            CONCATENATE 'CONVERSION_EXIT_' ls_dd01l-convexit '_INPUT'
            INTO lv_exit_name.
            IF sy-subrc = 0.

              CALL FUNCTION lv_exit_name
                EXPORTING
                  input  = iv_value
                IMPORTING
                  output = <value>.

              " return value
              ev_value = <value>.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD get_filter.

    IF context_e IS INITIAL. EXIT. ENDIF.

    DATA(lo_filter) = context_e->get_filter( ).
    DATA(lt_filter) = lo_filter->get_filter_select_options( ).

    TRANSLATE iv_field TO UPPER CASE.

    READ TABLE lt_filter INTO DATA(ls_filter) WITH KEY property = iv_field.

    IF sy-subrc IS NOT INITIAL. EXIT. ENDIF.

    LOOP AT ls_filter-select_options ASSIGNING FIELD-SYMBOL(<fs_so>).

      IF ib_conversion EQ abap_true.
        me->conversion_input( EXPORTING iv_value = <fs_so>-low
                                        iv_field = iv_field
                              IMPORTING ev_value = <fs_so>-low ).
      ENDIF.

      ev_value = <fs_so>-low.
    ENDLOOP.

    lo_filter->convert_select_option( EXPORTING is_select_option = ls_filter
                                      IMPORTING et_select_option = er_range ).

  ENDMETHOD.


  METHOD get_key.

    IF context_r IS INITIAL. EXIT. ENDIF.

    DATA(lt_keys) = context_r->get_keys( ).

    TRANSLATE iv_field TO UPPER CASE.

    READ TABLE lt_keys ASSIGNING FIELD-SYMBOL(<fs_key>) WITH KEY name = iv_field.
    IF sy-subrc EQ 0.
      ev_value = <fs_key>-value.
    ENDIF.

    IF ib_conversion EQ abap_true.
      me->conversion_input( EXPORTING iv_value = ev_value
                                      iv_field = iv_field
                            IMPORTING ev_value = ev_value ).
    ENDIF.


  ENDMETHOD.


  METHOD get_skip.

    IF context_e IS INITIAL. EXIT. ENDIF.

    rv_top = context_e->get_skip( ).
  ENDMETHOD.


  METHOD get_top.

    IF context_e IS INITIAL. EXIT. ENDIF.

    rv_top = context_e->get_top( ).
  ENDMETHOD.


  METHOD handle_sorting.

    IF context_e IS INITIAL. EXIT. ENDIF.

    DATA(lt_order) = context_e->get_orderby( ).

    IF lt_order[] IS INITIAL. EXIT. ENDIF.

    LOOP AT lt_order INTO DATA(ls_order).

      TRANSLATE ls_order-property TO UPPER CASE.
      CASE ls_order-order.
        WHEN 'desc'.
          ev_orderby = |{ ev_orderby } { ls_order-property } DESCENDING|.
        WHEN OTHERS.
          ev_orderby = |{ ev_orderby } { ls_order-property } ASCENDING|.
      ENDCASE.

    ENDLOOP.

    SHIFT ev_orderby LEFT DELETING LEADING space.

  ENDMETHOD.


  METHOD raise_exception.

    DATA: lv_msg TYPE string.

    IF iv_text1 IS NOT INITIAL.
      lv_msg = |{ iv_text1 }|.
    ENDIF.

    IF iv_text2 IS NOT INITIAL.
      lv_msg = |{ lv_msg } { iv_text2 }|.
    ENDIF.

    IF iv_text3 IS NOT INITIAL.
      lv_msg = |{ lv_msg } { iv_text3 }|.
    ENDIF.

    IF iv_text4 IS NOT INITIAL.
      lv_msg = |{ lv_msg } { iv_text4 }|.
    ENDIF.

    IF lv_msg IS INITIAL.
      lv_msg = TEXT-e01.
    ENDIF.

    RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
      EXPORTING
        textid            = /iwbep/cx_mgw_busi_exception=>business_error_unlimited
        message_unlimited = lv_msg.

  ENDMETHOD.


  METHOD replace_turkish_characters.

    REPLACE ALL OCCURRENCES OF:
     'ı' IN cv_text WITH 'i',
     'ğ' IN cv_text WITH 'g',
     'Ğ' IN cv_text WITH 'G',
     'ü' IN cv_text WITH 'u',
     'Ü' IN cv_text WITH 'U',
     'ş' IN cv_text WITH 's',
     'Ş' IN cv_text WITH 'S',
     'İ' IN cv_text WITH 'I',
     'ö' IN cv_text WITH 'o',
     'Ö' IN cv_text WITH 'O',
     'ç' IN cv_text WITH 'c',
     'Ç' IN cv_text WITH 'C'.

  ENDMETHOD.
ENDCLASS.
