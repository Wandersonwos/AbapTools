*"* use this source file for your ABAP unit test classes
*
*CLASS cl_unittest DEFINITION FOR TESTING RISK LEVEL HARMLESS.
*
*  PUBLIC SECTION.
*    METHODS check_methods FOR TESTING.
*  PRIVATE SECTION.
*    METHODS create_filled_range_ident RETURNING VALUE(rt_range_ident) TYPE dmc_mc_t_ident_range .
*    METHODS get_exp_range_ident_result
*      IMPORTING iv_sign               TYPE ddsign
*                iv_option             TYPE ddoption
*                iv_low                TYPE csequence
*                iv_high               TYPE csequence
*      RETURNING VALUE(rs_range_ident) TYPE dmc_mc_s_ident_range .
*    METHODS test_add_value_incl_cp_range IMPORTING it_range_ident TYPE dmc_mc_t_ident_range .
*    METHODS test_add_value_incl_eq_range IMPORTING it_range_ident TYPE dmc_mc_t_ident_range .
*    METHODS test_add_value_incl_ne_range IMPORTING it_range_ident TYPE dmc_mc_t_ident_range .
*    METHODS test_add_value_excl_eq_range IMPORTING it_range_ident TYPE dmc_mc_t_ident_range .
*    METHODS test_range_table_result
*      IMPORTING
*        it_range_ident TYPE dmc_mc_t_ident_range
*        iv_sign        TYPE ddsign
*        iv_option      TYPE ddoption
*        iv_low         TYPE csequence
*        iv_high        TYPE csequence.
*
*
*ENDCLASS.
*
*CLASS cl_unittest IMPLEMENTATION.
*
*
*  METHOD check_methods.
*
*    DATA lt_range_ident TYPE dmc_mc_t_ident_range.
*
*    "test if range table is empty
*    test_add_value_incl_cp_range( it_range_ident = lt_range_ident ).
*    test_add_value_incl_eq_range( it_range_ident = lt_range_ident ).
*    test_add_value_incl_ne_range( it_range_ident = lt_range_ident ).
*    test_add_value_excl_eq_range( it_range_ident = lt_range_ident ).
*
*    "test if range table is filled
*    lt_range_ident = create_filled_range_ident( ).
*    test_add_value_incl_cp_range( it_range_ident = lt_range_ident ).
*    test_add_value_incl_eq_range( it_range_ident = lt_range_ident ).
*    test_add_value_incl_ne_range( it_range_ident = lt_range_ident ).
*    test_add_value_excl_eq_range( it_range_ident = lt_range_ident ).
*
*  ENDMETHOD.
*
*  METHOD create_filled_range_ident.
*
*    DATA ls_range_ident TYPE dmc_mc_s_ident_range.
*
*    ls_range_ident-sign = 'I'.
*    ls_range_ident-option = 'CP'.
*    ls_range_ident-low = '1'.
*    ls_range_ident-high = space.
*    APPEND ls_range_ident TO rt_range_ident.
*
*    ls_range_ident-sign = 'I'.
*    ls_range_ident-option = 'CP'.
*    ls_range_ident-low = '2'.
*    ls_range_ident-high = space.
*    APPEND ls_range_ident TO rt_range_ident.
*
*    ls_range_ident-sign = 'I'.
*    ls_range_ident-option = 'CP'.
*    ls_range_ident-low = '3'.
*    ls_range_ident-high = space.
*    APPEND ls_range_ident TO rt_range_ident.
*
*  ENDMETHOD.
*
*  METHOD test_add_value_incl_cp_range.
*
*    DATA lt_range_ident TYPE dmc_mc_t_ident_range.
*
*
*    lt_range_ident = it_range_ident.
*
*    cl_dmc_ui_range_utils=>add_value_to_incl_cp_range(
*      EXPORTING
*        iv_value = 'LOW'
*      CHANGING
*        ct_range = lt_range_ident ).
*
*    test_range_table_result(
*      EXPORTING
*        it_range_ident = lt_range_ident
*        iv_sign = 'I'
*        iv_option = 'CP'
*        iv_low = 'LOW'
*        iv_high = space ).
*
*  ENDMETHOD.
*
*  METHOD test_add_value_incl_eq_range.
*
*    DATA lt_range_ident TYPE dmc_mc_t_ident_range.
*
*
*    lt_range_ident = it_range_ident.
*
*    cl_dmc_ui_range_utils=>add_value_to_incl_eq_range(
*      EXPORTING
*        iv_value = 'LOW'
*      CHANGING
*        ct_range = lt_range_ident ).
*
*    test_range_table_result(
*      EXPORTING
*        it_range_ident = lt_range_ident
*        iv_sign = 'I'
*        iv_option = 'EQ'
*        iv_low = 'LOW'
*        iv_high = space ).
*
*  ENDMETHOD.
*
*  METHOD test_add_value_incl_ne_range.
*
*    DATA lt_range_ident TYPE dmc_mc_t_ident_range.
*
*
*    lt_range_ident = it_range_ident.
*
*    cl_dmc_ui_range_utils=>add_value_to_incl_ne_range(
*      EXPORTING
*        iv_value = 'LOW'
*      CHANGING
*        ct_range = lt_range_ident ).
*
*    test_range_table_result(
*      EXPORTING
*        it_range_ident = lt_range_ident
*        iv_sign = 'I'
*        iv_option = 'NE'
*        iv_low = 'LOW'
*        iv_high = space ).
*
*  ENDMETHOD.
*
*  METHOD test_add_value_excl_eq_range.
*
*    DATA lt_range_ident TYPE dmc_mc_t_ident_range.
*
*
*    lt_range_ident = it_range_ident.
*
*    cl_dmc_ui_range_utils=>add_value_to_excl_eq_range(
*      EXPORTING
*        iv_value = 'LOW'
*      CHANGING
*        ct_range = lt_range_ident ).
*
*    test_range_table_result(
*      EXPORTING
*        it_range_ident = lt_range_ident
*        iv_sign = 'E'
*        iv_option = 'EQ'
*        iv_low = 'LOW'
*        iv_high = space ).
*
*  ENDMETHOD.
*
*  METHOD test_range_table_result.
*
*    DATA lv_lines TYPE i.
*    DATA lv_count TYPE i VALUE 0.
*    DATA ls_expected_result TYPE dmc_mc_s_ident_range.
*
*    FIELD-SYMBOLS <ls_range> TYPE dmc_mc_s_ident_range.
*
*
*    lv_lines = lines( it_range_ident ).
*
*    CASE lv_lines.
*      WHEN 0.
*        cl_aunit_assert=>fail( msg = 'Range table has no entries' ).
*      WHEN OTHERS.
*        LOOP AT it_range_ident ASSIGNING <ls_range>
*          WHERE sign = iv_sign
*          AND option = iv_option
*          AND low = iv_low
*          AND high = iv_high.
*
*          lv_count = lv_count + 1.
*        ENDLOOP.
*
*        IF lv_count > 1.
*          cl_aunit_assert=>fail( msg ='The range table has double entries' ).
*        ENDIF.
*
*    ENDCASE.
*
*    READ TABLE it_range_ident ASSIGNING <ls_range> WITH TABLE KEY sign = iv_sign option = iv_option low = iv_low high = iv_high.
*    IF sy-subrc <> 0.
*      cl_aunit_assert=>fail( msg = 'Expected entry in range table does not exists' ).
*    ENDIF.
*    ls_expected_result = get_exp_range_ident_result( iv_sign = iv_sign  iv_option = iv_option iv_low = iv_low iv_high = iv_high ).
*
*    IF <ls_range>-sign <> ls_expected_result-sign.
*      cl_aunit_assert=>fail( msg = 'The range table field SIGN is not filled correctly' ).
*    ENDIF.
*    IF <ls_range>-option <> ls_expected_result-option.
*      cl_aunit_assert=>fail( msg = 'The range table field OPTION is not filled correctly' ).
*    ENDIF.
*
*    IF <ls_range>-low <> ls_expected_result-low.
*      cl_aunit_assert=>fail( msg = 'The range table field LOW is not filled correctly' ).
*    ENDIF.
*    IF <ls_range>-high <> ls_expected_result-high.
*      cl_aunit_assert=>fail( msg = 'The range table field HIGH is not filled correctly' ).
*    ENDIF.
*
*  ENDMETHOD.
*
*  METHOD get_exp_range_ident_result.
*
*    rs_range_ident-sign = iv_sign.
*    rs_range_ident-option = iv_option.
*    rs_range_ident-low = iv_low.
*    rs_range_ident-high = iv_high.
*
*  ENDMETHOD.
*
*ENDCLASS.
