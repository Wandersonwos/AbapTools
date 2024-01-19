CLASS zcl_wda_tools DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    CONSTANTS:
      cs_id              TYPE string VALUE 'ID' ##no_text,
      cs_context_element TYPE string VALUE 'CONTEXT_ELEMENT' ##no_text.

    CLASS-METHODS:
      down_shortcut_sap_transaction
        IMPORTING
          i_filename          TYPE string OPTIONAL
          i_user              TYPE syuname DEFAULT sy-uname
          i_transaction       TYPE sy-tcode
          i_parameters        TYPE uid_name_val_tab
          i_title             TYPE char80 OPTIONAL
          i_not_auth_rfc_user TYPE abap_bool DEFAULT abap_true,
      open_dms_doc_on_sap_gui
        IMPORTING
          i_dokar TYPE draw-dokar
          i_doknr TYPE draw-doknr
          i_doktl TYPE draw-doktl
          i_dokvr TYPE draw-dokvr
          i_user  TYPE syuname DEFAULT sy-uname,
      download_table_as_xlsx
        IMPORTING
          i_table       TYPE table
          i_filename    TYPE string OPTIONAL
          it_fieldcat   TYPE lvc_t_fcat OPTIONAL
          it_sort       TYPE lvc_t_sort OPTIONAL
          it_filt       TYPE lvc_t_filt OPTIONAL
          is_layout     TYPE lvc_s_layo OPTIONAL
          it_hyperlinks TYPE lvc_t_hype OPTIONAL,
      download_excel
        IMPORTING
          i_filename TYPE string OPTIONAL
          i_data     TYPE xstring,
      handle_timeout_http_request
        IMPORTING
          i_server TYPE REF TO if_http_server,
      get_index_from_event_element
        IMPORTING
          io_wd_custom_event TYPE REF TO cl_wd_custom_event
        RETURNING
          VALUE(rv_index)    TYPE i,
      get_portal_origin
        RETURNING
          VALUE(rv_portal_origin) TYPE wdr_name_value-value,
      create_shortcut_sap_trans
        IMPORTING
          i_user              TYPE syuname OPTIONAL
          i_transaction       TYPE sy-tcode
          i_parameters        TYPE uid_name_val_tab
          i_title             TYPE char80
          i_not_auth_rfc_user TYPE abap_bool DEFAULT abap_true
        EXPORTING
          e_content           TYPE xstring
        CHANGING
          c_filename          TYPE string,
      conv_string_2_wd_formated_text
        IMPORTING
          text_string             TYPE string
        RETURNING
          VALUE(wd_formated_text) TYPE string,
      get_real_wd_user
        IMPORTING
          fixed_user          TYPE sy-uname OPTIONAL
        RETURNING
          VALUE(real_wd_user) TYPE sy-uname.

  PRIVATE SECTION.

    CLASS-DATA mv_portal_origin TYPE wdr_name_value-value.

ENDCLASS.


CLASS zcl_wda_tools IMPLEMENTATION.


  METHOD download_excel.

    DATA lv_filename TYPE string.

*    CALL FUNCTION 'SDOK_MIMETYPE_GET'
*      EXPORTING
*        extension = 'XLSX'
*      IMPORTING
*        mimetype  = lv_mimetype.

    DATA(lv_mimetype) = CONV string( zcl_file_tools=>get_mimetype_from_extension( 'XLSX' ) ).

    IF i_filename IS NOT INITIAL.
      lv_filename = i_filename  && '.xlsx'.
    ELSE.
      lv_filename = 'Export_' && sy-uname && '_' && sy-datum && '_' && sy-uzeit && '.xlsx'.
    ENDIF.

    cl_wd_runtime_services=>attach_file_to_response( i_filename      = lv_filename
                                                     i_content       = i_data
                                                     i_mime_type     = lv_mimetype
                                                     i_in_new_window = abap_false         ).
  ENDMETHOD.

  METHOD download_table_as_xlsx.


    DATA(lv_xstring) = zcl_itab_tools=>create_xlsx_in_xstring(
                                itdata         = i_table
                                it_fieldcat    = it_fieldcat
                                it_sort        = it_sort
                                it_filt        = it_filt
                                is_layout      = is_layout
                                it_hyperlinks  = it_hyperlinks ).

    download_excel( i_filename = i_filename
                    i_data     = lv_xstring ).

  ENDMETHOD.


  METHOD handle_timeout_http_request.

    DATA:
      lr_server  TYPE REF TO cl_http_server,
      lt_path    TYPE string_table,
      ls_appl    TYPE ztb_http_timeout,                     "#EC NEEDED
      ls_page    TYPE icf_response_page,
      lv_index   TYPE i,
      lv_service TYPE string.

    lr_server ?= i_server.
    CHECK lr_server IS BOUND.

    "cl_wdr_task=>application->name can NOT be used here, as the instance is already destroyed…
    SPLIT lr_server->m_runtime_memory_id AT `/` INTO TABLE lt_path.

    CHECK lt_path IS NOT INITIAL.

    DESCRIBE TABLE lt_path LINES lv_index.
    READ TABLE lt_path INTO lv_service INDEX lv_index.

    CHECK sy-subrc = 0.

    TRANSLATE lv_service TO UPPER CASE.

    SELECT SINGLE *
     FROM ztb_http_timeout
     INTO ls_appl
     WHERE service_name = lv_service
         AND active     = abap_true.

    CHECK sy-subrc = 0.

    ls_page-header = ls_appl-header_response_page.
    ls_page-body   = ls_appl-body_response_page.

    i_server->set_page(
      EXPORTING
        response_page_type   = i_server->co_page_error_type
        response_option_page = ls_page
      EXCEPTIONS
        invalid_parameter    = 1
        document_not_found   = 2
        OTHERS               = 3 ).


  ENDMETHOD.


  METHOD open_dms_doc_on_sap_gui.

    DATA(lt_parameters) = VALUE uid_name_val_tab( ( name  = 'DRAW-DOKNR' value = i_doknr )
                                                  ( name  = 'DRAW-DOKAR' value = i_dokar )
                                                  ( name  = 'DRAW-DOKTL' value = i_doktl )
                                                  ( name  = 'DRAW-DOKVR' value = i_dokvr ) ).
    zcl_wda_tools=>down_shortcut_sap_transaction(
      EXPORTING
        i_filename    =  |DMS { i_dokar } { i_doknr ALPHA = OUT } { i_doktl } { i_dokvr }|
        i_transaction =  '*CV03N'
        i_parameters  = lt_parameters
        i_title       = TEXT-001
        i_user        = i_user
    ).

  ENDMETHOD.


  METHOD down_shortcut_sap_transaction.

    DATA(lv_filename) = i_filename.

    create_shortcut_sap_trans(
      EXPORTING
        i_user              = i_user
        i_transaction       = i_transaction
        i_parameters        = i_parameters
        i_title             = i_title
        i_not_auth_rfc_user = i_not_auth_rfc_user
      IMPORTING
        e_content           = DATA(lx_conteudo_xstring)
      CHANGING
        c_filename          = lv_filename          ).

    cl_wd_runtime_services=>attach_file_to_response( i_filename      = lv_filename
                                                     i_in_new_window = abap_false
                                                     i_content       = lx_conteudo_xstring
                                                     i_mime_type     = zcl_file_tools=>get_mimetype_from_extension( if_ocs_process_constants=>c_sap ) ).
  ENDMETHOD.
  METHOD get_index_from_event_element.

    TRY.
        rv_index = io_wd_custom_event->get_context_element( cs_context_element )->get_index( ).
      CATCH cx_root.
    ENDTRY.
  ENDMETHOD.

  METHOD get_portal_origin.

    IF mv_portal_origin IS NOT INITIAL.
      rv_portal_origin = mv_portal_origin.

    ELSE.

      TRY.
          IF wdr_task=>client_window                     IS BOUND AND
             wdr_task=>client_window->client_info_object IS BOUND.

            TRY.
                rv_portal_origin  = wdr_task=>client_window->client_info_object->header_fields[ name = cl_rso_mmr_constants_attr=>n_c_att_origin ]-value.

              CATCH cx_sy_itab_line_not_found.
                TRY.
                    rv_portal_origin = wdr_task=>client_window->client_info_object->header_fields[ name = 'referer' ]-value.
                    DATA(lv_referer) = abap_true.

                  CATCH cx_sy_itab_line_not_found.
                    rv_portal_origin  = wdr_task=>client_window->client_info_object->header_fields[ name = if_xi_adapter_const_http=>co_attr_host ]-value.
                ENDTRY.
            ENDTRY.

            IF rv_portal_origin CS 'http://bspprd.refer.pt/sap/bc/webdynpro/sap/'.
              rv_portal_origin = 'http://portal.refer.pt'.
            ENDIF.

            FIND cl_cim_constants=>host_prefix IN rv_portal_origin MATCH OFFSET DATA(lv_posicao).
            IF sy-subrc = 0.
              SHIFT rv_portal_origin LEFT BY lv_posicao + 2 PLACES.
            ENDIF.

            IF lv_referer = abap_true.
              SHIFT rv_portal_origin RIGHT DELETING TRAILING if_gho_constants=>gc_slash.
              CONDENSE rv_portal_origin NO-GAPS.
            ENDIF.

            mv_portal_origin = rv_portal_origin.

          ENDIF.
        CATCH cx_sy_itab_line_not_found.
      ENDTRY.
    ENDIF.

  ENDMETHOD.


  METHOD create_shortcut_sap_trans.

    DATA:
      parameter           TYPE text255,
      content_file_string TYPE string,
      ticket              TYPE string,
      v_inicio            TYPE string,
      v_final             TYPE string,
      lv_transacao        TYPE tstc-tcode,
      lw_parameters       LIKE LINE OF i_parameters.

    IF i_transaction(1) = '*'.
      lv_transacao = i_transaction+1.
    ELSE.
      lv_transacao = i_transaction.
    ENDIF.

    SELECT COUNT(*)
    FROM tstc
    WHERE tcode EQ lv_transacao.

    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

* Populate the parameters to be passed to the shortcut
    IF NOT i_parameters IS INITIAL.
      CLEAR parameter.
      LOOP AT i_parameters INTO lw_parameters.
        CONCATENATE parameter lw_parameters-name '=' lw_parameters-value ';' INTO parameter.
      ENDLOOP.
    ENDIF.

*** create the shortcut content for the required transaction
    CALL FUNCTION 'SWN_CREATE_SHORTCUT'
      EXPORTING
        i_transaction           = i_transaction
        i_parameter             = parameter
        i_user                  = i_user
        i_windowsize            = 'Maximized window' "'Normal window'
        i_title                 = i_title
      IMPORTING
        shortcut_string         = content_file_string
      EXCEPTIONS
        inconsistent_parameters = 1
        OTHERS                  = 2.

    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    IF i_not_auth_rfc_user = abap_false AND sy-uname(4) NP '*RFC*'.

      CALL FUNCTION 'CREATE_RFC_REENTRANCE_TICKET'
        IMPORTING
          ticket = ticket.

      SPLIT content_file_string AT '[Function]' INTO v_inicio v_final IN CHARACTER MODE.

      content_file_string = v_inicio
                            && 'at="MYSAPSSO2='
                            && ticket  && '"'
                            && cl_abap_char_utilities=>cr_lf
                            && '[Function]'
                            && v_final.

    ENDIF.

    CALL FUNCTION 'J_3RT_CONV_STRING_TO_XSTRING'
      EXPORTING
        im_string  = content_file_string
      IMPORTING
        ex_xstring = e_content.

    IF c_filename IS NOT INITIAL.
      c_filename = c_filename && '.SAP'.
    ELSE.
      c_filename = 'Tcode_' && sy-uname && '_' && sy-datum && '_' && sy-uzeit && '.SAP'.
    ENDIF.

  ENDMETHOD.

  METHOD conv_string_2_wd_formated_text.

    DATA lt_tline TYPE tline_tab.

    CALL FUNCTION 'CONVERT_STREAM_TO_ITF_TEXT'
      EXPORTING
        stream_lines = VALUE string_table( ( text_string ) )
        lf           = abap_true
      TABLES
        itf_text     = lt_tline.

    wd_formated_text = cl_wd_formatted_text=>create_from_sapscript( lt_tline )->m_xml_text.

*    ls_new_notes-notes_content = cl_fpm_guibb_utils=>get_plain_text( iv_formatted_text = ls_new_notes-notes_content ).
*     special formating for WDY *********************************************************


*      l_r_text = cl_wd_formatted_text=>create_from_sapscript(
*        sapscript_head  = l_s_head
*        sapscript_lines = l_t_text ).
*      r_text = l_r_text->m_xml_text.


*cl_wd_formatted_text=>create_from_html(
*  EXPORTING
*    html           =
**    link_mode      = e_link_mode-remove_links
**    opener_context = if_wdr_rr_application=>e_link_opener_context-default
**  RECEIVING
**    formatted_text =
*).

*ms_interven-chefe_trabalhos_str = cl_wd_formatted_text=>make_text(
**  EXPORTING
**    formatted_text_before =
**    inner_formatted_text  =
*    inner_text            = ms_interven-chefe_trabalhos_str
**    tag                   = cl_wd_formatted_text=>e_tag-br
**    formatted_text_after  =
**  RECEIVING
**    formatted_text        =
*)->m_xml_text.

*    DATA(lo_formated_text) = cl_wd_formatted_text=>create(
*      EXPORTING
*        xml_text       = ms_interven-chefe_trabalhos_str
**    type           =
**    opener_context = if_wdr_rr_application=>e_link_opener_context-default
**  RECEIVING
**    formatted_text =
*    ).
*
*    DATA f1 TYPE REF TO cl_wd_formatted_text.
*    DATA f2 TYPE REF TO cl_wd_formatted_text.
*    DATA f3 TYPE REF TO cl_wd_formatted_text.
*    DATA f  TYPE REF TO cl_wd_formatted_text.
*    f1 = cl_wd_formatted_text=>make_text(
*     inner_text = 'available'                               "#EC NOTEXT
*          tag        = cl_wd_formatted_text=>e_tag-em   ).
*    f2 = cl_wd_formatted_text=>make_sap_field( name = 'CONNID' ).
*    f2 = cl_wd_formatted_text=>make_text( inner_formatted_text= f2
*                                          tag = cl_wd_formatted_text=>e_tag-code ).
*    f3 = cl_wd_formatted_text=>make_sap_field( name = 'CARRID' ).
*    f = cl_wd_formatted_text=>make_text( inner_text = 'Places &STATE& for flight &CONNID& of line &CARRID&' ). "#EC NOTEXT
*
*    f->replace_placeholder( name = 'STATE'  formatted_text = f1 ).
*    f->replace_placeholder( name = 'CONNID' formatted_text = f2 ).
*    f->replace_placeholder( name = 'CARRID' formatted_text = f3 ).
*    wd_context->set_attribute( name = 'TEXT' value = f->m_xml_text ).
*
*    IF lo_formated_text->m_xml_text IS NOT INITIAL.
*
*    ENDIF.

  ENDMETHOD.

  METHOD get_real_wd_user.
    TRY.
        CASE fixed_user.
          WHEN zcl_sigma_user_info=>c_fixed_user.
            DATA(lo_sigma_wd_user) = zcl_sigma_user_info=>get_instance( ).
            IF lo_sigma_wd_user IS BOUND.
              real_wd_user = lo_sigma_wd_user->get_sigma_user( ).
            ENDIF.

          WHEN zcl_ppt_ad_user_info=>c_fixed_user.
            DATA(lo_ppt_wd_user) = zcl_ppt_ad_user_info=>get_instance( ).
            IF lo_ppt_wd_user IS BOUND.
              real_wd_user = lo_ppt_wd_user->get_ppt_user( ).
            ENDIF.
          WHEN 'ERFC_EDIFIC'.

        ENDCASE.
      CATCH cx_root.
    ENDTRY.
  ENDMETHOD.

ENDCLASS.
