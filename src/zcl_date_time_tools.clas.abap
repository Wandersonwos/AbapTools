CLASS zcl_date_time_tools DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    TYPES:
      BEGIN OF ty_week_info,
        week       TYPE scal-week,
        begin_date TYPE tstrgenseg-begin_date,
        end_date   TYPE tstrgenseg-end_date,
      END OF ty_week_info,
      tty_week_info TYPE SORTED TABLE OF ty_week_info WITH UNIQUE KEY week,
      BEGIN OF ty_weeks_from_year,
        year  TYPE tstr_year,
        weeks TYPE tty_week_info,
      END   OF ty_weeks_from_year,
      tty_weeks_from_year TYPE STANDARD TABLE OF ty_weeks_from_year WITH DEFAULT KEY.

    CLASS-METHODS:
      "! Check if a moment is before other one
      "! @parameter start_date | Start date of first moment
      "! @parameter start_time | Start time of first moment
      "! @parameter finish_date | Finish date of first moment
      "! @parameter finish_time | Finish time of first moment
      "! @parameter equal_is_ok | Indicate that equality is ok
      "! @parameter round_sec | Indicate that seconds must be ignored
      "! @parameter r_ok |
      check_date_time_period
        IMPORTING
          start_date  TYPE d
          start_time  TYPE t
          finish_date TYPE d
          finish_time TYPE t
          equal_is_ok TYPE abap_bool DEFAULT abap_true
          ignore_sec  TYPE abap_bool DEFAULT abap_true
        RETURNING
          VALUE(r_ok) TYPE abap_bool,

      check_week
        IMPORTING
          week        TYPE kweek OPTIONAL
          week_string TYPE string OPTIONAL
          limit_10_years type abap_bool DEFAULT abap_true
            PREFERRED PARAMETER week
        RAISING
          cx_t100_msg,
      week_get_first_day
        IMPORTING
          week        TYPE kweek
        RETURNING
          VALUE(date) TYPE scdatum,
      switch_weeks
        IMPORTING
          i_week        TYPE kweek
          i_quantity    TYPE i
        RETURNING
          VALUE(r_week) TYPE kweek,
      get_week_info_based_on_date
        IMPORTING
          date   TYPE sy-datum DEFAULT sy-datum
        EXPORTING
          week   TYPE scal-week
          monday TYPE sy-datum
          sunday TYPE sy-datum,
      get_week_first_day
        IMPORTING
          VALUE(week) TYPE scal-week
        EXPORTING
          VALUE(date) TYPE scal-date
        EXCEPTIONS
          week_invalid,
      get_qty_weeks_from_year
        IMPORTING
          year             TYPE gjahr
        RETURNING
          VALUE(qty_weeks) TYPE pie_nweks,
      get_month_name
        IMPORTING
          mounth            TYPE t247-mnr
        RETURNING
          VALUE(mouth_name) TYPE t247-ltx,
      get_qty_weeks_from_month
        IMPORTING
          year             TYPE gjahr
          mounth_number    TYPE t247-mnr
          start_of_week    TYPE i DEFAULT 4  " Thursday
        RETURNING
          VALUE(qty_weeks) TYPE pie_nweks,
      get_month_names
        RETURNING
          VALUE(mouth_names) TYPE ftps_web_month_t,
      get_week_dates
        IMPORTING
          i_week                TYPE scal-week
        RETURNING
          VALUE(ls_semana_info) TYPE ty_week_info,
      get_weeks_from_year
        IMPORTING
          year            TYPE scal-year
        RETURNING
          VALUE(rt_weeks) TYPE zcl_date_time_tools=>ty_weeks_from_year-weeks,
      round_time
        CHANGING
          c_date TYPE sy-datum DEFAULT sy-datum
          c_time TYPE sy-uzeit DEFAULT sy-uzeit,

      get_weekday_text
        IMPORTING
          date           TYPE d
          language       TYPE syst_langu DEFAULT sy-langu
        RETURNING
          VALUE(weekday) TYPE  t246.

  PRIVATE SECTION.

    CLASS-DATA:
      mt_weeks_info_from_year TYPE tty_weeks_from_year,
      mt_month_desc           TYPE ftps_web_month_t.

    CLASS-METHODS load_weeks_from_year
      IMPORTING
        year TYPE scal-year.

ENDCLASS.



CLASS zcl_date_time_tools IMPLEMENTATION.


  METHOD check_date_time_period.

    DATA:
      lv_dif_secs    TYPE fahztd,
      lv_date2_early TYPE selkz.

    TRY.

        DATA(lv_start_time)  = start_time.
        DATA(lv_finish_time) = finish_time.

        IF  ignore_sec = abap_true.

          lv_start_time+4(2) = '00'.
          lv_finish_time+4(2) = '00'.

        ENDIF.

        CALL FUNCTION 'SD_CALC_DURATION_FROM_DATETIME'
          EXPORTING
            i_date1          = start_date
            i_time1          = lv_start_time
            i_date2          = finish_date
            i_time2          = lv_finish_time
          IMPORTING
            e_tdiff          = lv_dif_secs
            e_date2_early    = lv_date2_early
          EXCEPTIONS
            invalid_datetime = 1
            OTHERS           = 2.

        IF lv_date2_early = abap_false AND ( equal_is_ok = abap_true OR lv_dif_secs IS NOT INITIAL ).
          r_ok = abap_true.
        ENDIF.

      CATCH cx_root.
    ENDTRY.

  ENDMETHOD.


  METHOD get_month_name.

    IF mt_month_desc IS INITIAL.
      get_month_names( ).
    ENDIF.

    TRY.
        mouth_name = mt_month_desc[ mnr = mounth ]-ltx.
      CATCH cx_sy_itab_line_not_found.
    ENDTRY.

  ENDMETHOD.


  METHOD get_month_names.

    IF mt_month_desc IS INITIAL.

      CALL FUNCTION 'MONTH_NAMES_GET'
        TABLES
          month_names           = mt_month_desc
        EXCEPTIONS
          month_names_not_found = 1
          OTHERS                = 2.
    ENDIF.

    mouth_names = mouth_names.

  ENDMETHOD.


  METHOD get_qty_weeks_from_month.

    DATA:
      lv_last_day    TYPE d,
      lv_first_day   TYPE d,
      lv_day_of_week TYPE cind.

    lv_first_day = year && mounth_number && '01'.

    CALL FUNCTION 'BKK_GET_MONTH_LASTDAY'
      EXPORTING
        i_date = lv_first_day
      IMPORTING
        e_date = lv_last_day.

    DATA(lv_date) = lv_first_day.

    WHILE lv_date <= lv_last_day.

      CALL FUNCTION 'DATE_COMPUTE_DAY'
        EXPORTING
          date = lv_date
        IMPORTING
          day  = lv_day_of_week.

      IF lv_day_of_week = start_of_week.
        qty_weeks = qty_weeks + 1.
        lv_date = lv_date + 7.
      ELSE.
        lv_date = lv_date + 1.
      ENDIF.

    ENDWHILE.

  ENDMETHOD.


  METHOD get_qty_weeks_from_year.

    DATA lv_last_week TYPE kweek.

    CALL FUNCTION 'TIME_GET_LAST_WEEK'
      EXPORTING
        if_year     = CONV i( year )
      IMPORTING
        ef_week     = lv_last_week
      EXCEPTIONS
        fatal_error = 1
        OTHERS      = 2.

    qty_weeks = lv_last_week+4(2).

  ENDMETHOD.


  METHOD get_weeks_from_year.

    TRY.

        rt_weeks = mt_weeks_info_from_year[ year = year ]-weeks.

      CATCH cx_sy_itab_line_not_found.

        load_weeks_from_year( year ).

        TRY.
            rt_weeks = mt_weeks_info_from_year[ year = year ]-weeks.
          CATCH cx_sy_itab_line_not_found.
        ENDTRY.
    ENDTRY.

  ENDMETHOD.


  METHOD get_week_dates.

    TRY.

        ls_semana_info = mt_weeks_info_from_year[ year = i_week(4) ]-weeks[ week = i_week ].

      CATCH cx_sy_itab_line_not_found.

        load_weeks_from_year( i_week(4) ).

        TRY.
            ls_semana_info = mt_weeks_info_from_year[ year = i_week(4) ]-weeks[ week = i_week ].
          CATCH cx_sy_itab_line_not_found.
        ENDTRY.

    ENDTRY.

  ENDMETHOD.


  METHOD get_week_first_day.

    date = get_week_dates( week )-begin_date.

    IF date IS INITIAL.

      MESSAGE e018(fopch) WITH week+4(2) week(4) RAISING week_invalid.
      "  A semana &1 não existe para o ano &2, entrar valor válido

    ENDIF.

  ENDMETHOD.


  METHOD get_week_info_based_on_date.

    DATA(lt_weeks) = get_weeks_from_year( date(4) ).

    LOOP AT lt_weeks REFERENCE INTO DATA(ls_week)
                     WHERE begin_date <= date
                       AND end_date   >= date.

      week   = date(4) && CONV numc2( sy-tabix ).
      monday = ls_week->begin_date.
      sunday = ls_week->end_date.

    ENDLOOP.

  ENDMETHOD.


  METHOD load_weeks_from_year.

    DATA lt_year_info TYPE tstr_gensegyeartab.

    CALL FUNCTION 'TSTR_PERIODS_WEEKS'
      EXPORTING
        it_yeartab   = VALUE tstr_yeartab( ( year ) )
        is_ttstr     = VALUE ttstr( )
      IMPORTING
        et_gensegtab = lt_year_info
      EXCEPTIONS
        error        = 1
        OTHERS       = 2.

    IF sy-subrc = 0.
      APPEND LINES OF VALUE tty_weeks_from_year( FOR ls IN lt_year_info
                                                  ( year = ls-year
                                                    weeks = VALUE #( FOR ls_week IN ls-gensegtab INDEX INTO week_index
                                                                     ( week       = year && CONV numc2( week_index )
                                                                       begin_date = ls_week-begin_date
                                                                       end_date   = ls_week-end_date - 1               ) ) ) )
                                                TO mt_weeks_info_from_year.
    ENDIF.

  ENDMETHOD.


  METHOD round_time.

    DATA(lv_time_old) = c_time.

    CALL FUNCTION 'WGRC_H_ROUND_TIME'
      EXPORTING
        i_round_minute = 1
      CHANGING
        c_date         = c_date
        c_time         = c_time.

    " Sometimes we need go back 1 minute
    IF lv_time_old+4(2) < 31 AND c_time > lv_time_old.

      TRY.
          cl_abap_tstmp=>td_add(
              EXPORTING
                date     = c_date
                time     = c_time
                secs     = '-60'
              IMPORTING
                res_date = c_date
                res_time = c_time
            ).
        CATCH cx_root.
      ENDTRY.

    ENDIF.
  ENDMETHOD.


  METHOD switch_weeks.

    IF i_quantity = 0.
      r_week = i_week.
    ELSE.

      DATA(lv_date) = week_get_first_day( i_week ).

      lv_date = cl_hrpad_date_computations=>add_weeks_to_date( start_date = lv_date
                                                               weeks      = i_quantity ).

      r_week = cl_rscrm_imp_time_utility=>get_week_from_date( lv_date ).

    ENDIF.

  ENDMETHOD.


  METHOD week_get_first_day.

    date = get_week_dates( week )-begin_date.

  ENDMETHOD.

  METHOD check_week.
    DATA lv_last_week TYPE kweek.

    DATA(lv_week) = week.

    IF lv_week IS INITIAL AND week_string IS NOT INITIAL.

      TRY.
          lv_week = week_string.

        CATCH cx_root.
          MESSAGE e363(zz_sigma) WITH week_string INTO sy-tvar9. zcl_message_tools=>raise_sy_msg_as_cx_t100_msg( ).
          " Semana &1 inválida. Por favor informe o ano + semana (AAAASS)
      ENDTRY.
    ENDIF.

    IF lv_week CN '0123456789' OR lv_week(4) < 1901 OR lv_week+4(2) < 1.
      MESSAGE e363(zz_sigma) WITH lv_week INTO sy-tvar9. zcl_message_tools=>raise_sy_msg_as_cx_t100_msg( ).
      " Semana &1 inválida. Por favor informe o ano + semana (AAAASS)
    ENDIF.

    IF lv_week+4(2) > 51.
      TRY.
          CALL FUNCTION 'BKK_GET_LASTWEEK_OF_YEAR'
            EXPORTING i_year = CONV i( lv_week(4) )
            IMPORTING e_week = lv_last_week.

        CATCH cx_root.
          MESSAGE e363(zz_sigma) WITH lv_week INTO sy-tvar9. zcl_message_tools=>raise_sy_msg_as_cx_t100_msg( ).
          " Semana &1 inválida. Por favor informe o ano + semana (AAAASS)
      ENDTRY.

      IF lv_week > lv_last_week.

        DATA(lv_first_week) = lv_last_week.

        lv_first_week+4(2) = '01'.

        " Semana &1 inválida. Para o ano &2 pode introduzir &3 a &4.
        MESSAGE e264(zz_sigma) WITH lv_week lv_week(4) lv_first_week lv_last_week INTO sy-tvar9.
        zcl_message_tools=>raise_sy_msg_as_cx_t100_msg( ).

      ENDIF.
    ENDIF.

    IF limit_10_years = abap_true AND lv_week(4) > CONV numc4(  sy-datum(4) + 10 ).
      " Semana &1 inválida. Escolha uma semana dentro dos próximos 10 anos.
      MESSAGE e463(zz_sigma) WITH lv_week INTO sy-tvar9.
      zcl_message_tools=>raise_sy_msg_as_cx_t100_msg( ).
    ENDIF.
  ENDMETHOD.

  METHOD get_weekday_text.

    DATA lt_weekday TYPE wgrc_t246_tty.

    CALL FUNCTION 'WEEKDAY_GET'
      EXPORTING
        language          = language
      TABLES
        weekday           = lt_weekday
      EXCEPTIONS
        weekday_not_found = 1
        OTHERS            = 2.

    IF sy-subrc = 0.
      weekday = VALUE #( lt_weekday[ wotnr = cl_rs_time_service=>get_weekday( date ) ] OPTIONAL ).
    ENDIF.

  ENDMETHOD.

ENDCLASS.
