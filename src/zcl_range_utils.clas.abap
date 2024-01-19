CLASS zcl_range_utils DEFINITION
  PUBLIC
  CREATE PUBLIC .

*"* public components of class CL_DMC_UI_RANGE_UTILS
*"* do not include other source files here!!!
  PUBLIC SECTION.
    TYPE-POOLS rsmds .

    CONSTANTS co_sign TYPE string VALUE 'SIGN'.             "#EC NOTEXT
    CONSTANTS co_option TYPE string VALUE 'OPTION'.         "#EC NOTEXT
    CONSTANTS co_low TYPE string VALUE 'LOW'.               "#EC NOTEXT
    CONSTANTS co_high TYPE string VALUE 'HIGH'.             "#EC NOTEXT
    CONSTANTS co_operator_and TYPE dmc_ui_operator_bt_where_cond VALUE 'AND'. "#EC NOTEXT
    CONSTANTS co_operator_or TYPE dmc_ui_operator_bt_where_cond VALUE 'OR'. "#EC NOTEXT

    CLASS-METHODS add_value_to_excl_eq_range
      IMPORTING
        !iv_value       TYPE any
      CHANGING
        VALUE(ct_range) TYPE table .
    CLASS-METHODS add_value_to_incl_cp_range
      IMPORTING
        !iv_value TYPE any
      CHANGING
        !ct_range TYPE table .
    CLASS-METHODS add_value_to_incl_eq_range
      IMPORTING
        !iv_value       TYPE any
      CHANGING
        VALUE(ct_range) TYPE table .
    CLASS-METHODS add_value_to_incl_ne_range
      IMPORTING
        !iv_value       TYPE any
      CHANGING
        VALUE(ct_range) TYPE table .
    CLASS-METHODS get_incl_excl_of_range
      IMPORTING
        !it_range      TYPE table
      EXPORTING
        !et_range_incl TYPE table
        !et_range_excl TYPE table .
    CLASS-METHODS translate_range_to_upper_case
      CHANGING
        !ct_range TYPE table .

  PRIVATE SECTION.

ENDCLASS.



CLASS zcl_range_utils IMPLEMENTATION.


  METHOD add_value_to_excl_eq_range.

    FIELD-SYMBOLS:
      <ls_range>     TYPE any,
      <lv_component> TYPE any.

    CHECK iv_value IS NOT INITIAL.

    APPEND INITIAL LINE TO ct_range ASSIGNING <ls_range>.

    ASSIGN COMPONENT co_sign OF STRUCTURE <ls_range> TO <lv_component>.
    CHECK sy-subrc = 0.
    <lv_component> = rsmds_c_sign-excluding.

    ASSIGN COMPONENT co_option OF STRUCTURE <ls_range> TO <lv_component>.
    CHECK sy-subrc = 0.
    <lv_component> = rsmds_c_option-equal.

    ASSIGN COMPONENT co_low OF STRUCTURE <ls_range> TO <lv_component>.
    CHECK sy-subrc = 0.
    <lv_component> = iv_value.

  ENDMETHOD.


  METHOD add_value_to_incl_cp_range.

    FIELD-SYMBOLS:
      <ls_range>     TYPE any,
      <lv_component> TYPE any.

    CHECK iv_value IS NOT INITIAL.

    APPEND INITIAL LINE TO ct_range ASSIGNING <ls_range>.

    ASSIGN COMPONENT co_sign OF STRUCTURE <ls_range> TO <lv_component>.
    CHECK sy-subrc = 0.
    <lv_component> = rsmds_c_sign-including.

    ASSIGN COMPONENT co_option OF STRUCTURE <ls_range> TO <lv_component>.
    CHECK sy-subrc = 0.
    <lv_component> = rsmds_c_option-contains_pattern.

    ASSIGN COMPONENT co_low OF STRUCTURE <ls_range> TO <lv_component>.
    CHECK sy-subrc = 0.
    <lv_component> = iv_value.

    FIND '*' IN iv_value.
    CHECK sy-subrc <> 0.

    <lv_component> = |*{ iv_value }*|.

  ENDMETHOD.


  METHOD add_value_to_incl_eq_range.

    FIELD-SYMBOLS:
      <ls_range>     TYPE any,
      <lv_component> TYPE any.

    CHECK iv_value IS NOT INITIAL.

    APPEND INITIAL LINE TO ct_range ASSIGNING <ls_range>.

    ASSIGN COMPONENT co_sign OF STRUCTURE <ls_range> TO <lv_component>.
    CHECK sy-subrc = 0.
    <lv_component> = rsmds_c_sign-including.

    ASSIGN COMPONENT co_option OF STRUCTURE <ls_range> TO <lv_component>.
    CHECK sy-subrc = 0.
    <lv_component> = rsmds_c_option-equal.

    ASSIGN COMPONENT co_low OF STRUCTURE <ls_range> TO <lv_component>.
    CHECK sy-subrc = 0.
    <lv_component> = iv_value.

  ENDMETHOD.


  METHOD add_value_to_incl_ne_range.

    FIELD-SYMBOLS:
      <ls_range>     TYPE any,
      <lv_component> TYPE any.

    CHECK iv_value IS NOT INITIAL.

    APPEND INITIAL LINE TO ct_range ASSIGNING <ls_range>.

    ASSIGN COMPONENT co_sign OF STRUCTURE <ls_range> TO <lv_component>.
    CHECK sy-subrc = 0.
    <lv_component> = rsmds_c_sign-including.

    ASSIGN COMPONENT co_option OF STRUCTURE <ls_range> TO <lv_component>.
    CHECK sy-subrc = 0.
    <lv_component> = rsmds_c_option-not_equal.

    ASSIGN COMPONENT co_low OF STRUCTURE <ls_range> TO <lv_component>.
    CHECK sy-subrc = 0.
    <lv_component> = iv_value.


  ENDMETHOD.


  METHOD get_incl_excl_of_range.

    FIELD-SYMBOLS:
      <ls_range> TYPE any,
      <lv_sign>  TYPE any.

    CLEAR:
      et_range_incl,
      et_range_excl.

    LOOP AT it_range ASSIGNING <ls_range>.
      ASSIGN COMPONENT 'SIGN' OF STRUCTURE <ls_range> TO <lv_sign>.
      CASE <lv_sign>.
        WHEN rsmds_c_sign-including.
          APPEND <ls_range> TO et_range_incl.
        WHEN rsmds_c_sign-excluding.
          APPEND <ls_range> TO et_range_excl.
      ENDCASE.
    ENDLOOP.

  ENDMETHOD.


  METHOD translate_range_to_upper_case.

    FIELD-SYMBOLS:
      <ls_range> TYPE any,
      <lv_data>  TYPE any.

    LOOP AT ct_range ASSIGNING <ls_range>.
      ASSIGN COMPONENT co_low OF STRUCTURE <ls_range> TO <lv_data>.
      TRANSLATE <lv_data> TO UPPER CASE.

      ASSIGN COMPONENT co_high OF STRUCTURE <ls_range> TO <lv_data>.
      TRANSLATE <lv_data> TO UPPER CASE.
    ENDLOOP.

  ENDMETHOD.
ENDCLASS.
