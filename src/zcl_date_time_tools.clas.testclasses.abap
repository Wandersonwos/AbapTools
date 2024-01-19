*"* use this source file for your ABAP unit test classes
CLASS ltcl_check_period DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS:
      first_test FOR TESTING RAISING cx_static_check.
ENDCLASS.


CLASS ltcl_check_period IMPLEMENTATION.

  METHOD first_test.

    CONSTANTS c_time_base TYPE t VALUE '195700'.

    DATA:
      lv_act TYPE t,
      lv_exp TYPE t.

*    lv_time = '195600'.
    lv_act = '195700'.
    lv_exp = '195931'.

    DATA(cut) = NEW zcl_date_time_tools( ).

    DO 200 TIMES.

      lv_act = c_time_base + sy-index.

      if     lv_act < '195731'.
        lv_exp = '195700'.
      ELSEIF lv_act < '195831'.
        lv_exp = '195800'.
      ELSEIF lv_act < '195931'.
        lv_exp = '195900'.
      ELSE.
        lv_exp = '200000'.
      ENDIF.

      cut->round_time( CHANGING c_time = lv_act ).

      cl_abap_unit_assert=>assert_equals(
        EXPORTING
          act  = lv_act
          exp  = lv_exp
          quit = if_aunit_constants=>no
          msg  = |Houve erro no horário { lv_act }| ).

    ENDDO.

  ENDMETHOD.

ENDCLASS.
