CLASS zcl_fpm_tools DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES:
      " Similar STANDARD class CL_HRESS_FPM_MSG_SERVICES
      BEGIN OF ty_search_crit_excel,
        fieldname TYPE fieldname,
        sign      TYPE tvarv_sign,
        option    TYPE string,
        low       TYPE rsdsselop_,
        high      TYPE rsdsselop_,
      END OF ty_search_crit_excel .
    TYPES:
      tty_search_crit_excel TYPE STANDARD TABLE OF ty_search_crit_excel WITH DEFAULT KEY .
    TYPES:
      BEGIN OF ty_search_criteria_values_set,
        fieldname TYPE fieldname.
    TYPES:
      values_set TYPE fpmgb_t_namevalue,
      END OF ty_search_criteria_values_set .
    TYPES:
      tty_search_criteria_values_set TYPE SORTED TABLE OF ty_search_criteria_values_set WITH UNIQUE KEY fieldname .
    TYPES:
      BEGIN OF ty_label_text,
        name       TYPE fpmgb_s_fieldusage-name,
        label_text TYPE fpmgb_s_fieldusage-label_text,
      END OF ty_label_text .
    TYPES:
      tty_label_text   TYPE SORTED TABLE OF ty_label_text WITH NON-UNIQUE KEY name,
      tty_fpm_event_id TYPE STANDARD TABLE OF fpm_event_id WITH EMPTY KEY.

    CONSTANTS:
      BEGIN OF c_fpm_evt,
        BEGIN OF move_list_item,
          up    TYPE fpm_event_id VALUE 'FPM_LIST_ITEM_MOVE_UP',
          down  TYPE fpm_event_id VALUE 'FPM_LIST_ITEM_MOVE_DOWN',
          first TYPE fpm_event_id VALUE 'FPM_LIST_ITEM_MOVE_FIRST',
          last  TYPE fpm_event_id VALUE 'FPM_LIST_ITEM_MOVE_LAST',
        END OF move_list_item,
        BEGIN OF uibb_panels,
          expand_all   TYPE fpm_event_id VALUE 'FPM_UIBB_PANELS_EXPAND_ALL',
          collapse_all TYPE fpm_event_id VALUE 'FPM_UIBB_PANELS_COLLAPSE_ALL',
        END OF uibb_panels,
      END OF c_fpm_evt,
      BEGIN OF c_fpm_list_formatter,
        BEGIN OF sigma,
          falhas TYPE abap_abstypename VALUE '\CLASS=ZCL_SIGMA_FALHAS_LIST\TYPE=TY_NOTES_OUTPUT_FPM_LIST',
          prog   TYPE abap_abstypename VALUE '\TYPE=ZSIGMA_IT_ORDERS_OUTPUT',
          valid  TYPE abap_abstypename VALUE '\TYPE=ZSIGMA_IT_VALID_OUTPUT',
        END OF sigma,
      END OF c_fpm_list_formatter.

    CLASS-METHODS get_move_list_event_ids
      RETURNING
        VALUE(rt_event_id) TYPE tty_fpm_event_id.
    CLASS-METHODS merge_search_crit_values_set
      IMPORTING
        !it_search_criteria_values_set TYPE tty_search_criteria_values_set
      CHANGING
        !ct_sel_opt_attributes         TYPE wdr_so_t_attributes .
    CLASS-METHODS add_exception_to_fpm_msg
      IMPORTING
        !io_exception TYPE REF TO cx_root
        !i_ref_name   TYPE clike OPTIONAL
        !i_ref_index  TYPE fpmgb_s_t100_message-ref_index OPTIONAL
        i_ref_names   TYPE fpmgb_t_ref_names OPTIONAL
      CHANGING
        !ct_message   TYPE fpmgb_t_messages .
    CLASS-METHODS add_bapi_msg_to_fpm_msg
      IMPORTING
        !it_bapi_msg TYPE bapiret2_tab
        !i_ref_name  TYPE clike OPTIONAL
        !i_ref_names TYPE fpmgb_t_ref_names OPTIONAL
        !i_ref_index TYPE fpmgb_s_t100_message-ref_index OPTIONAL
      CHANGING
        !ct_message  TYPE fpmgb_t_messages .
    CLASS-METHODS add_bapi_msg_to_fpm_event_msg
      IMPORTING
        line_index   TYPE i OPTIONAL
        low          TYPE boole_d  OPTIONAL
        high         TYPE boole_d  OPTIONAL
        !it_bapi_msg TYPE bapiret2_tab
      CHANGING
        !ct_message  TYPE fpmgb_search_t_t100_message .
    CLASS-METHODS add_syst_msg_to_fpm_msg
      IMPORTING
        !i_ref_name  TYPE clike OPTIONAL
        !i_ref_names TYPE fpmgb_t_ref_names OPTIONAL
        !i_ref_index TYPE fpmgb_s_t100_message-ref_index OPTIONAL
      CHANGING
        !ct_message  TYPE fpmgb_t_messages .
    CLASS-METHODS add_syst_msg_to_fpm_search_msg
      IMPORTING
        !i_message_index TYPE fpmgb_s_t100_message-ref_index OPTIONAL
      CHANGING
        !ct_message      TYPE fpmgb_search_t_t100_message .
    CLASS-METHODS transfer_severity
      IMPORTING
        !iv_msgty          TYPE msgty
      RETURNING
        VALUE(rv_severity) TYPE fpm_message_severity .
    CLASS-METHODS set_fpm_title
      IMPORTING
        !i_title    TYPE fpm_content_area_title
        !i_ovp_page TYPE fpm_content_area_id .
    CLASS-METHODS conv_set_to_fpm_fixed_values
      IMPORTING
        !i_set_values         TYPE fpmgb_t_namevalue
      RETURNING
        VALUE(r_fixed_values) TYPE wdr_context_attr_value_list .
    CLASS-METHODS get_key_from_on_sel_dd_event
      IMPORTING
        !io_event  TYPE REF TO cl_fpm_event
      RETURNING
        VALUE(key) TYPE string .
    CLASS-METHODS conv_fpm_search_crit_to_excel
      IMPORTING
        !it_sel_opt_attributes    TYPE wdr_so_t_attributes
        !it_fpm_search_criteria   TYPE fpmgb_t_search_criteria
      RETURNING
        VALUE(rt_search_criteria) TYPE trsy_claspos_selopt .
    CLASS-METHODS get_excel_fcat_search_result
      IMPORTING
        !it_field_usage   TYPE fpmgb_t_fieldusage
        !it_search_result TYPE STANDARD TABLE
      EXPORTING
        !et_field_catalog TYPE zexcel_t_fieldcatalog .
    CLASS-METHODS get_excel_from_search_guibb
      IMPORTING
        !it_sel_opt_attributes  TYPE wdr_so_t_attributes OPTIONAL
        !it_fpm_search_criteria TYPE fpmgb_t_search_criteria OPTIONAL
        !it_field_catalog       TYPE zexcel_t_fieldcatalog OPTIONAL
        !it_search_criteria     TYPE trsy_claspos_selopt OPTIONAL
        !i_title                TYPE zexcel_sheet_title OPTIONAL
        !i_tab_color            TYPE zexcel_s_tabcolor OPTIONAL
      RETURNING
        VALUE(ro_excel)         TYPE REF TO zcl_excel
      RAISING
        zcx_excel .
    CLASS-METHODS get_include_operators
      RETURNING
        VALUE(rt_include_operators) TYPE fpmgb_t_search_operator .
    CLASS-METHODS remove_all_oper_except_one
      IMPORTING
        !i_operator_exception      TYPE wdr_so_operator
      RETURNING
        VALUE(et_exclude_operator) TYPE fpmgb_t_search_operator .
    CLASS-METHODS navegate_to_wda
      IMPORTING
        !i_wd_application   TYPE apb_lpd_string
        !i_wd_configuration TYPE apb_lpd_string
        !it_param           TYPE apb_lpd_t_params OPTIONAL   ##NO_TEXT
        !i_sap_language_p   TYPE abap_bool DEFAULT abap_true
        !i_navigation_mode  TYPE apb_lpd_nav_mode DEFAULT 'EXTERNAL'
        !i_portal_url       TYPE string OPTIONAL
      RAISING
        cx_t100_msg .
    CLASS-METHODS navegate_to_url
      IMPORTING
        !i_url             TYPE string
        !i_navigation_mode TYPE apb_lpd_nav_mode DEFAULT 'EXTERNAL'
        !it_param          TYPE apb_lpd_t_params OPTIONAL .
    CLASS-METHODS get_excel_fcat_search_crit
      IMPORTING
        !it_search_criteria     TYPE trsy_claspos_selopt
      RETURNING
        VALUE(rt_field_catalog) TYPE zexcel_t_fieldcatalog .
    CLASS-METHODS create_range_fpm_search_atrib
      IMPORTING
        !io_search_conversion TYPE REF TO if_fpm_guibb_search_conversion
        !iv_search_attribute  TYPE fpmgb_s_search_criteria-search_attribute
      EXPORTING
        !et_range             TYPE REF TO data .
    CLASS-METHODS set_operator_data_range
      CHANGING
        c_data_range TYPE trty_rdate_range.
    CLASS-METHODS get_table_paste_data
      IMPORTING
        event            TYPE REF TO cl_fpm_event
      EXPORTING
        table_paste_data TYPE wdui_table_paste_data.
    CLASS-METHODS move_list_item
      IMPORTING
        iv_raised_by_own_ui       TYPE boole_d OPTIONAL
        io_event                  TYPE REF TO cl_fpm_event
      EXPORTING
        et_messages               TYPE fpmgb_t_messages
        ev_selected_lines_changed TYPE boole_d
      CHANGING
        ct_index_table            TYPE STANDARD TABLE
        ct_selected_lines         TYPE rstabixtab.
    CLASS-METHODS
      is_event_move_list
        IMPORTING
          mv_event_id                  TYPE fpm_event_id
        RETURNING
          VALUE(rv_is_event_move_list) TYPE abap_bool.
    CLASS-METHODS:
      expand_colaps_all_painels
        IMPORTING
          io_ovp TYPE REF TO if_fpm_ovp.
    CLASS-METHODS:
      fpm_exp_excel_list_formatter
        IMPORTING
          ir_data            TYPE REF TO data
        CHANGING
          cr_cell_properties TYPE REF TO cl_salv_bs_lex_h_cell_props=>ys_cell_properties.


  PRIVATE SECTION.

    CLASS-METHODS conv_search_criteria
      IMPORTING
        it_search_criteria          TYPE trsy_claspos_selopt
      RETURNING
        VALUE(rt_search_crit_excel) TYPE zcl_fpm_tools=>tty_search_crit_excel.

ENDCLASS.



CLASS zcl_fpm_tools IMPLEMENTATION.


  METHOD add_bapi_msg_to_fpm_event_msg.

    " Get rid of duplicates
    DATA(lt_bapi_msg) = it_bapi_msg.

    SORT lt_bapi_msg BY                                   id type number message_v1 message_v2 message_v3 message_v4.
    DELETE ADJACENT DUPLICATES FROM lt_bapi_msg COMPARING id type number message_v1 message_v2 message_v3 message_v4.

    " Transfer messages
    LOOP AT lt_bapi_msg ASSIGNING FIELD-SYMBOL(<ls_bapi_msg>).

      APPEND VALUE #( msgid       = <ls_bapi_msg>-id
                      msgno       = <ls_bapi_msg>-number
                      parameter_1 = <ls_bapi_msg>-message_v1
                      parameter_2 = <ls_bapi_msg>-message_v2
                      parameter_3 = <ls_bapi_msg>-message_v3
                      parameter_4 = <ls_bapi_msg>-message_v4
                      line_index  = line_index
                      low         = low
                      high        = high
                      severity    = transfer_severity( <ls_bapi_msg>-type ) ) TO ct_message.
    ENDLOOP.

  ENDMETHOD.


  METHOD add_bapi_msg_to_fpm_msg.

    DATA lt_bapi_msg LIKE it_bapi_msg.

    CHECK it_bapi_msg IS NOT INITIAL.

    " Get rid of duplicates
    lt_bapi_msg = it_bapi_msg.
    SORT lt_bapi_msg BY                                   id type number message_v1 message_v2 message_v3 message_v4.
    DELETE ADJACENT DUPLICATES FROM lt_bapi_msg COMPARING id type number message_v1 message_v2 message_v3 message_v4.

    LOOP AT lt_bapi_msg REFERENCE INTO DATA(ls_bapi_msg).

      APPEND VALUE #( msgid                        = ls_bapi_msg->id
                      msgno                        = ls_bapi_msg->number
                      parameter_1                  = ls_bapi_msg->message_v1
                      parameter_2                  = ls_bapi_msg->message_v2
                      parameter_3                  = ls_bapi_msg->message_v3
                      parameter_4                  = ls_bapi_msg->message_v4
                      ref_name                     = i_ref_name
                      ref_names                    = i_ref_names
                      ref_index                    = i_ref_index
                      is_enable_message_navigation = COND #( WHEN i_ref_name  IS NOT INITIAL OR
                                                                  i_ref_index IS NOT INITIAL
                                                             THEN abap_true )
                      severity    = transfer_severity( ls_bapi_msg->type ) ) TO ct_message.
    ENDLOOP.

  ENDMETHOD.


  METHOD add_exception_to_fpm_msg.

    DATA lt_msg_bapiret2 TYPE bapiret2_tab.

    zcl_message_tools=>exception_to_bapiret2(
          EXPORTING
            io_exception = io_exception
          CHANGING
            ct_bapiret2  = lt_msg_bapiret2 ).

    add_bapi_msg_to_fpm_msg(
      EXPORTING
        i_ref_name  = i_ref_name
        i_ref_names = i_ref_names
        i_ref_index = i_ref_index
        it_bapi_msg = lt_msg_bapiret2
      CHANGING
        ct_message  = ct_message    ).

  ENDMETHOD.


  METHOD add_syst_msg_to_fpm_search_msg.

    APPEND VALUE #( msgid         = sy-msgid
                    msgno         = sy-msgno
                    parameter_1   = sy-msgv1
                    parameter_2   = sy-msgv2
                    parameter_3   = sy-msgv3
                    parameter_4   = sy-msgv4
                    message_index = i_message_index
                    severity      = transfer_severity( sy-msgty ) ) TO ct_message.
  ENDMETHOD.


  METHOD add_syst_msg_to_fpm_msg.

    " Put into FPM log
    APPEND INITIAL LINE TO ct_message REFERENCE INTO DATA(ls_fpm_log_msg).

    " Move parameters
    ls_fpm_log_msg->msgid       = sy-msgid.
    ls_fpm_log_msg->msgno       = sy-msgno.
    ls_fpm_log_msg->parameter_1 = sy-msgv1.
    ls_fpm_log_msg->parameter_2 = sy-msgv2.
    ls_fpm_log_msg->parameter_3 = sy-msgv3.
    ls_fpm_log_msg->parameter_4 = sy-msgv4.

    " Transfer severity
    ls_fpm_log_msg->severity = transfer_severity( sy-msgty ).

    IF i_ref_name  IS NOT INITIAL OR
       i_ref_names IS NOT INITIAL OR
       i_ref_index IS NOT INITIAL.

      ls_fpm_log_msg->ref_name                     = i_ref_name.
      ls_fpm_log_msg->ref_names                    = i_ref_names.
      ls_fpm_log_msg->ref_index                    = i_ref_index.
      ls_fpm_log_msg->is_enable_message_navigation = abap_true.

    ENDIF.

  ENDMETHOD.


  METHOD conv_fpm_search_crit_to_excel.

    DATA(lt_sel_opt_attrib) = it_sel_opt_attributes.

    SORT lt_sel_opt_attrib BY disp_index.

    LOOP AT it_fpm_search_criteria REFERENCE INTO DATA(ls_fpm_search_criteria).

      TRY.
          DATA(ls_sel_opt_attrib) = REF #( lt_sel_opt_attrib[ attribute = ls_fpm_search_criteria->search_attribute ] ).
        CATCH cx_sy_itab_line_not_found.
          CONTINUE.
      ENDTRY.

      CHECK ls_fpm_search_criteria->low              IS NOT INITIAL OR
            ls_fpm_search_criteria->high             IS NOT INITIAL OR
            ls_fpm_search_criteria->multi_value_attr IS NOT INITIAL.

      APPEND INITIAL LINE TO rt_search_criteria REFERENCE INTO DATA(ls_search_crit_excel).

      TRY.
          ls_search_crit_excel->* = CORRESPONDING #( cl_fpm_guibb_search_conversion=>to_abap_select_option( ls_fpm_search_criteria->* ) ).
        CATCH cx_fpmgb.
          CONTINUE.
      ENDTRY.

      ls_search_crit_excel->fieldname = ls_sel_opt_attrib->text.

      IF ls_sel_opt_attrib->value_set IS NOT INITIAL.

        TRY.
            IF ls_fpm_search_criteria->low IS NOT INITIAL.
              ls_search_crit_excel->low  = ls_sel_opt_attrib->value_set[ value = ls_fpm_search_criteria->low  ]-text.
            ENDIF.
            IF ls_fpm_search_criteria->high IS NOT INITIAL.
              ls_search_crit_excel->high = ls_sel_opt_attrib->value_set[ value = ls_fpm_search_criteria->high ]-text.
            ENDIF.
          CATCH cx_sy_itab_line_not_found.
        ENDTRY.

      ENDIF.

      IF ls_sel_opt_attrib->rel_date IS NOT INITIAL.

        IF ls_fpm_search_criteria->low IS NOT INITIAL.
          DATA(lv_date_low) = CONV d( ls_fpm_search_criteria->low ).
          ls_search_crit_excel->low = |{ lv_date_low DATE = ENVIRONMENT }|.
        ENDIF.

        IF ls_fpm_search_criteria->high IS NOT INITIAL.
          DATA(lv_date_high) = CONV d( ls_fpm_search_criteria->high ).
          ls_search_crit_excel->high = |{ lv_date_high DATE = ENVIRONMENT }|.
        ENDIF.

      ENDIF.

    ENDLOOP.

  ENDMETHOD.


  METHOD conv_search_criteria.

    DATA lt_search_crit_excel TYPE tty_search_crit_excel.

    LOOP AT it_search_criteria REFERENCE INTO DATA(ls_search_crit).

      APPEND CORRESPONDING #( ls_search_crit->* EXCEPT option ) TO rt_search_crit_excel REFERENCE INTO DATA(ls_search_crit_excel).

      ls_search_crit_excel->option = SWITCH #( ls_search_crit->option
                                               WHEN if_drf_const=>range_option_equal     THEN TEXT-005
                                               WHEN if_drf_const=>range_option_between   THEN TEXT-006
                                               WHEN if_drf_const=>range_option_not_equal THEN TEXT-007
                                               WHEN if_drf_const=>range_option_cp        THEN TEXT-008 ).
    ENDLOOP.

  ENDMETHOD.


  METHOD conv_set_to_fpm_fixed_values.

    r_fixed_values = CORRESPONDING #( i_set_values MAPPING text = name ).

  ENDMETHOD.


  METHOD create_range_fpm_search_atrib.

    FIELD-SYMBOLS <ft_sel_opt> TYPE table.

    " create data referene for a ABAP range table
    io_search_conversion->create_abap_range_ref(
       EXPORTING
         iv_search_attribute = iv_search_attribute
       IMPORTING
         er_range_tab_ref    = et_range ).

    " convert search data into ABAP select statements, i.e. fill
    io_search_conversion->fpm_attribute_into_abap_range(
       EXPORTING
         iv_search_attribute = iv_search_attribute
       IMPORTING
         et_range_tab_ref    = et_range ).

    ASSIGN et_range->* TO <ft_sel_opt>.

    LOOP AT <ft_sel_opt> ASSIGNING FIELD-SYMBOL(<ls_range>).

      ASSIGN COMPONENT cl_dmc_ui_range_utils=>co_option OF STRUCTURE <ls_range> TO FIELD-SYMBOL(<lv_option>).
      CHECK sy-subrc = 0 AND <lv_option> = if_fsbp_const_range=>option_not_equal.

      ASSIGN COMPONENT cl_dmc_ui_range_utils=>co_sign OF STRUCTURE <ls_range> TO FIELD-SYMBOL(<lv_sign>).
      CHECK sy-subrc = 0 AND <lv_sign> = if_cwd_constants=>c_sign_inclusive.

      <lv_sign>   = if_cwd_constants=>c_sign_exclusive.
      <lv_option> = if_cwd_constants=>c_option_equals.

    ENDLOOP.

  ENDMETHOD.


  METHOD get_excel_fcat_search_crit.

    rt_field_catalog = zcl_excel_common=>get_fieldcatalog( it_search_criteria ).


    LOOP AT rt_field_catalog REFERENCE INTO DATA(ls_field_cat).

      CASE ls_field_cat->fieldname.

        WHEN 'FIELDNAME'.
          ls_field_cat->scrtext_m = TEXT-001.

        WHEN 'SIGN'.
          ls_field_cat->dynpfld = abap_false.

        WHEN 'OPTION'.
          ls_field_cat->scrtext_m = TEXT-002.

        WHEN 'LOW'.
          ls_field_cat->scrtext_m = TEXT-003.

        WHEN 'HIGH'.
          ls_field_cat->scrtext_m = TEXT-004.

      ENDCASE.

    ENDLOOP.

  ENDMETHOD.


  METHOD get_excel_fcat_search_result.

    et_field_catalog = zcl_excel_common=>get_fieldcatalog( it_search_result ).

    LOOP AT et_field_catalog REFERENCE INTO DATA(ls_field_catalog).

      READ TABLE it_field_usage WITH KEY name = ls_field_catalog->fieldname REFERENCE INTO DATA(ls_field_usage).
      IF sy-subrc = 0.

        DATA(lv_label_text) = ls_field_usage->label_text.

        REPLACE cl_abap_char_utilities=>newline IN lv_label_text WITH ` `.

        ls_field_catalog->position  = sy-tabix.
        IF strlen( lv_label_text ) <= 10.
          ls_field_catalog->scrtext_s = lv_label_text.
        ELSE.
          CLEAR ls_field_catalog->scrtext_s.
        ENDIF.

        IF strlen( lv_label_text ) <= 20.
          ls_field_catalog->scrtext_m = lv_label_text.
        ELSE.
          CLEAR ls_field_catalog->scrtext_m.
        ENDIF.

        ls_field_catalog->scrtext_l = lv_label_text.

        ls_field_catalog->dynpfld   = abap_true.
      ELSE.
        ls_field_catalog->dynpfld   = abap_false.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.


  METHOD get_excel_from_search_guibb.

    DATA:
      lt_search_criteria TYPE trsy_claspos_selopt,
      lt_field_catalog   TYPE zexcel_t_fieldcatalog.

    IF it_search_criteria IS INITIAL.
      lt_search_criteria = conv_fpm_search_crit_to_excel( it_sel_opt_attributes  = it_sel_opt_attributes
                                                          it_fpm_search_criteria = it_fpm_search_criteria ).
    ELSE.
      lt_search_criteria = it_search_criteria.
    ENDIF.

    IF it_field_catalog IS INITIAL.
      lt_field_catalog = get_excel_fcat_search_crit( lt_search_criteria ).
    ELSE.
      lt_field_catalog = it_field_catalog.
    ENDIF.

    ro_excel = NEW #( ).

    " Primeiro separador: Critérios de Filtragem
    DATA(lo_worksheet) = ro_excel->get_active_worksheet( ).

    DATA(lv_title) = i_title.
    IF lv_title  IS INITIAL.
      lv_title = CONV #( TEXT-009 ).
    ENDIF.

    lo_worksheet->set_title( lv_title ).
    lo_worksheet->set_column_width( ip_column = 'A' ip_width_fix = 40 ).
    lo_worksheet->set_column_width( ip_column = 'B' ip_width_fix = 40 ).
    lo_worksheet->set_column_width( ip_column = 'C' ip_width_fix = 40 ).
    lo_worksheet->set_column_width( ip_column = 'D' ip_width_fix = 40 ).

    " Set color to tab with sheetname   -

    DATA(ls_tab_color) = i_tab_color.
    IF ls_tab_color IS INITIAL.
      ls_tab_color = VALUE #( rgb =  zcl_excel_style_color=>c_green ).
    ENDIF.

    DATA(lt_search_crit_excel) = conv_search_criteria( lt_search_criteria ).

    lo_worksheet->set_tabcolor( ls_tab_color ).
    lo_worksheet->bind_table( ip_table         = lt_search_crit_excel
                              it_field_catalog = lt_field_catalog       ).

  ENDMETHOD.


  METHOD get_include_operators.

    rt_include_operators = VALUE #( ( operator_id = if_wd_select_options_20=>e_operators-is         )
                                    ( operator_id = if_wd_select_options_20=>e_operators-is_between ) ).
  ENDMETHOD.


  METHOD get_key_from_on_sel_dd_event.

    DATA lt_wdr_event_parameter_list TYPE wdr_event_parameter_list.

    io_event->mo_event_data->get_value(
      EXPORTING
        iv_key   = cl_fpm_event=>gc_event_param_wd_event_params
      IMPORTING
        ev_value = lt_wdr_event_parameter_list
    ).

    TRY.
        DATA(lv_key) = lt_wdr_event_parameter_list[ name = if_ua_attribute=>key ]-value.

        ASSIGN lv_key->* TO FIELD-SYMBOL(<lv_key>).

        key = <lv_key>.

      CATCH cx_sy_itab_line_not_found.
    ENDTRY.

  ENDMETHOD.


  METHOD merge_search_crit_values_set.

    LOOP AT it_search_criteria_values_set REFERENCE INTO DATA(ls_filtro_dd_valores).
      TRY.
          DATA(lt_value_set_ref) = REF #( ct_sel_opt_attributes[ attribute = ls_filtro_dd_valores->fieldname ]-value_set ).
          APPEND LINES OF ls_filtro_dd_valores->values_set TO lt_value_set_ref->*.
        CATCH cx_sy_itab_line_not_found.
      ENDTRY.

    ENDLOOP.

  ENDMETHOD.


  METHOD navegate_to_url.

    CHECK i_url IS NOT INITIAL.

    DATA(lo_navigate_to) = cl_fpm=>get_instance( )->get_navigate_to( ).

    IF wdr_task=>client_window->client_info_object->client_environment = if_wdr_client_info_object=>e_client_environment-portal.

      lo_navigate_to->launch_portal_page( is_additional_parameters = VALUE #( navigation_mode = i_navigation_mode )
                                          is_portal_page_fields    = VALUE #( path_to_page = i_url
                                                                              parameter    = it_param )            ).
    ELSE.

      lo_navigate_to->launch_url( is_additional_parameters = VALUE #( navigation_mode = i_navigation_mode )
                                  is_url_fields            = VALUE #( url       = i_url
                                                                      parameter = it_param )               ).
    ENDIF.

  ENDMETHOD.


  METHOD navegate_to_wda.

    DATA lv_url TYPE string.

    DATA(lt_param) = it_param.

    IF wdr_task=>client_window->client_info_object->client_environment = if_wdr_client_info_object=>e_client_environment-portal.

      lv_url = i_portal_url.

    ELSE.

      cl_wd_utilities=>construct_wd_url(
        EXPORTING
          application_name = i_wd_application  " Application
        IMPORTING
          out_absolute_url = lv_url    ).      " Absolute URL (Incl. Log, Host, Port)

      APPEND VALUE #( key   = cl_wdr_client_constants=>config_id
                      value = i_wd_configuration                 ) TO lt_param.
    ENDIF.

    navegate_to_url( it_param = lt_param
                     i_url    = lv_url   ).

  ENDMETHOD.


  METHOD remove_all_oper_except_one.

    DO.
      ASSIGN COMPONENT sy-index OF STRUCTURE if_wd_select_options_20=>e_operators TO FIELD-SYMBOL(<lv_id_operator>).
      IF sy-subrc <> 0.
        EXIT.
      ENDIF.

      CHECK <lv_id_operator> <> i_operator_exception.

      APPEND VALUE #( operator_id = <lv_id_operator> ) TO et_exclude_operator.

    ENDDO.

  ENDMETHOD.


  METHOD set_fpm_title.

    DATA(lo_fpm_ovp) = CAST if_fpm_cnr_ovp( cl_fpm=>get_instance( )->get_service( if_fpm_constants=>gc_service_key-cnr_ovp ) ).

    CHECK lo_fpm_ovp IS BOUND.

    TRY.
        lo_fpm_ovp->change_content_area_restricted( iv_content_area_id = i_ovp_page
                                                    iv_title           = i_title ).
      CATCH cx_fpm_floorplan.
    ENDTRY.

  ENDMETHOD.


  METHOD transfer_severity.

    DATA:
      lv_severity TYPE fpm_message_severity.

* Init retunring parameter
    CLEAR rv_severity.

* Check importing parameter
    IF iv_msgty IS INITIAL.
      RETURN.
    ENDIF.

* Transfer severity
    CASE iv_msgty.
      WHEN if_lo_oif_constants=>c_amsg OR
           if_lo_oif_constants=>c_xmsg.
*     Fatal error: Raise an online dump immediately
        ASSERT 1 = 2.
      WHEN if_lo_oif_constants=>c_emsg.
*     Error
        lv_severity = if_fpm_message_manager=>gc_severity_error.
      WHEN if_lo_oif_constants=>c_wmsg OR
           if_lo_oif_constants=>c_imsg.
*     Warning
        lv_severity = if_fpm_message_manager=>gc_severity_warning.
      WHEN OTHERS.
*     Success
        lv_severity = if_fpm_message_manager=>gc_severity_success.
    ENDCASE.

* Set retunring parameter
    rv_severity = lv_severity.

  ENDMETHOD.
  METHOD set_operator_data_range.

    LOOP AT c_data_range REFERENCE INTO DATA(ls_data_range).

      IF NOT ls_data_range->low IS INITIAL.

        IF NOT ls_data_range->high IS INITIAL.
          ls_data_range->option = if_cwd_constants=>c_option_between.
        ELSE.
          ls_data_range->option = if_cwd_constants=>c_option_greater_than_equals.
        ENDIF.

      ELSEIF NOT ls_data_range->high IS INITIAL.

        ls_data_range->option = if_cwd_constants=>c_option_less_than_equals.
        ls_data_range->low    = ls_data_range->high.
        CLEAR ls_data_range->high.

      ENDIF.

      IF ls_data_range->sign IS INITIAL.
        ls_data_range->sign = if_cwd_constants=>c_sign_inclusive.
      ENDIF.

    ENDLOOP.
  ENDMETHOD.

  METHOD get_table_paste_data.

    FIELD-SYMBOLS:
      <lt_wdr_event_param_list> TYPE wdr_event_parameter_list,
      <lt_table_paste_data>     LIKE table_paste_data.

    CHECK event IS BOUND AND event->mv_event_id = if_fpm_guibb_list=>gc_event_multi_value_paste.

    event->mo_event_data->get_value(
      EXPORTING
        iv_key   = if_fpm_constants=>gc_event_param-wd_event_params
      IMPORTING
        er_value = DATA(lt_wdr_event_param_list)
    ).

    ASSIGN lt_wdr_event_param_list->* TO <lt_wdr_event_param_list>.
    CHECK sy-subrc = 0.

    TRY.
        DATA(lt_table_paste_data) = <lt_wdr_event_param_list>[ name = if_vch_const_attr_key=>table ]-value.
      CATCH cx_sy_itab_line_not_found.
        RETURN.
    ENDTRY.

    ASSIGN lt_table_paste_data->* TO <lt_table_paste_data>.
    CHECK sy-subrc = 0.

    table_paste_data = <lt_table_paste_data>.

  ENDMETHOD.

  METHOD move_list_item.

    FIELD-SYMBOLS <ls_line_ref_old> TYPE any.

    DATA:
      lv_tabix_new TYPE i,
      ls_line_ref  TYPE REF TO data.

    CHECK iv_raised_by_own_ui = abap_true AND
          is_event_move_list( io_event->mv_event_id ).

    DATA(lv_qtd_lines)          = lines( ct_index_table    ).
    DATA(lv_qtd_lines_selected) = lines( ct_selected_lines ).

    IF lv_qtd_lines_selected <> 1.

      MESSAGE e218(idcn) WITH lv_qtd_lines_selected INTO sy-tvar9.
      " Selecione somente uma linha; você selecionou &1 linhas
      zcl_fpm_tools=>add_syst_msg_to_fpm_msg( CHANGING ct_message  = et_messages ).
      RETURN.
    ENDIF.

    DATA(lv_tabix_old) = ct_selected_lines[ 1 ]-tabix.

    CASE io_event->mv_event_id.

      WHEN c_fpm_evt-move_list_item-up OR
           c_fpm_evt-move_list_item-first.

        IF lv_tabix_old = 1.
          RETURN.
        ENDIF.

      WHEN c_fpm_evt-move_list_item-down OR
           c_fpm_evt-move_list_item-last.

        IF lv_tabix_old = lv_qtd_lines.
          RETURN.
        ENDIF.

    ENDCASE.

    lv_tabix_new = SWITCH #( io_event->mv_event_id
                             WHEN c_fpm_evt-move_list_item-up    THEN lv_tabix_old - 1
                             WHEN c_fpm_evt-move_list_item-down  THEN lv_tabix_old + 1
                             WHEN c_fpm_evt-move_list_item-first THEN 1
                             WHEN c_fpm_evt-move_list_item-last  THEN lv_qtd_lines    ).


    CREATE DATA ls_line_ref LIKE LINE OF ct_index_table.

    ASSIGN ls_line_ref->* TO FIELD-SYMBOL(<ls_line_ref>).

    ASSIGN ct_index_table[ lv_tabix_old ] TO <ls_line_ref_old>.
    <ls_line_ref> = <ls_line_ref_old>.

    DELETE ct_index_table INDEX lv_tabix_old.
    INSERT <ls_line_ref> INTO ct_index_table INDEX lv_tabix_new.

    ct_selected_lines[ 1 ]-tabix = lv_tabix_new.

    ev_selected_lines_changed = abap_true.

  ENDMETHOD.

  METHOD is_event_move_list.

    rv_is_event_move_list = xsdbool( mv_event_id = c_fpm_evt-move_list_item-up    OR
                                     mv_event_id = c_fpm_evt-move_list_item-first OR
                                     mv_event_id = c_fpm_evt-move_list_item-down  OR
                                     mv_event_id = c_fpm_evt-move_list_item-last    ).

  ENDMETHOD.

  METHOD get_move_list_event_ids.

    rt_event_id = VALUE #( ( c_fpm_evt-move_list_item-up    )
                           ( c_fpm_evt-move_list_item-first )
                           ( c_fpm_evt-move_list_item-down  )
                           ( c_fpm_evt-move_list_item-last  ) ).
  ENDMETHOD.

  METHOD expand_colaps_all_painels.

    DATA(lo_event) = io_ovp->get_event( ).

    CASE lo_event->mv_event_id.

      WHEN c_fpm_evt-uibb_panels-expand_all OR
           c_fpm_evt-uibb_panels-collapse_all.
        TRY.
            io_ovp->get_uibbs( IMPORTING et_uibb = DATA(lt_uibb) ).

            LOOP AT lt_uibb REFERENCE INTO DATA(ls_uibb).
              ls_uibb->collapsed = xsdbool( lo_event->mv_event_id = c_fpm_evt-uibb_panels-collapse_all ).
              io_ovp->change_uibb( ls_uibb->* ).
            ENDLOOP.
          CATCH cx_fpm_floorplan.
        ENDTRY.
    ENDCASE.

  ENDMETHOD.

  METHOD fpm_exp_excel_list_formatter.

    CHECK cr_cell_properties->field_attributes-name = 'NUMERO_AVARIA' OR
          cr_cell_properties->field_attributes-name = 'AUFNR'.

    TRY.

        DATA(lo_structdescr) = CAST cl_abap_structdescr( cl_abap_tabledescr=>describe_by_data_ref( ir_data ) ).

        CASE lo_structdescr->absolute_name.

          WHEN c_fpm_list_formatter-sigma-prog
            OR c_fpm_list_formatter-sigma-valid.

            CHECK cr_cell_properties->field_attributes-name = 'AUFNR'.

            ASSIGN ir_data->* TO FIELD-SYMBOL(<ls_data>).

            ASSIGN COMPONENT 'AUFNR_SIGMA_LINK' OF STRUCTURE <ls_data> TO FIELD-SYMBOL(<lv_aufnr_sigma_link>).
            CHECK sy-subrc = 0.

            cr_cell_properties->hyperlink = <lv_aufnr_sigma_link>.

          WHEN c_fpm_list_formatter-sigma-falhas.

            FIELD-SYMBOLS <ls_falha> TYPE zcl_sigma_falhas_list=>ty_notes_output_fpm_list.
            ASSIGN ir_data->* TO <ls_falha>.

            CASE cr_cell_properties->field_attributes-name.
              WHEN 'NUMERO_AVARIA'.
                cr_cell_properties->hyperlink = <ls_falha>-qmnum_sigma_link.
              WHEN 'AUFNR'.
                cr_cell_properties->hyperlink = <ls_falha>-aufnr_sigma_link.
            ENDCASE.

        ENDCASE.

      CATCH cx_root.
    ENDTRY.
  ENDMETHOD.

ENDCLASS.
