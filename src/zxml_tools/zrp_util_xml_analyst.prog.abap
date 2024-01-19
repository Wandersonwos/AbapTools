*&---------------------------------------------------------------------*
*& Report  zrp_util_xml_analyst
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT zrp_util_xml_analyst.


PARAMETER:
 p_file TYPE localfile OBLIGATORY.

SELECTION-SCREEN SKIP.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-006.
PARAMETER:
 p_ex_par TYPE trexs_parameter_value-parameter LOWER CASE DEFAULT 'Note',
 p_ex_val TYPE trexs_parameter_value-value     LOWER CASE DEFAULT 'TAXAS E IMPOSTOS'.

SELECTION-SCREEN END OF BLOCK b1.

CLASS lcl_main DEFINITION CREATE PRIVATE.

  PUBLIC SECTION.

    CLASS-METHODS:
        get_directory,
        run.

  PRIVATE SECTION.

    CLASS-METHODS:
      get_desktop_directory
        RETURNING
          value(r_desktop_directory) TYPE string,
      alv_config.

    CLASS-DATA: go_alv TYPE REF TO cl_salv_table,
                go_util_xml_analyst TYPE REF TO zcl_util_xml_analyst.

ENDCLASS.

CLASS lcl_main IMPLEMENTATION.

  METHOD get_directory.

    DATA:
         lv_desktop_directory TYPE string,
         lt_file_table TYPE filetable,
         lv_rc TYPE i.

    lv_desktop_directory = get_desktop_directory( ).

    cl_gui_frontend_services=>file_open_dialog(
      EXPORTING
        file_filter             = cl_gui_frontend_services=>filetype_xml
        initial_directory       = lv_desktop_directory
      CHANGING
        file_table              = lt_file_table
        rc                      = lv_rc
      EXCEPTIONS
        file_open_dialog_failed = 1
        cntl_error              = 2
        error_no_gui            = 3
        not_supported_by_gui    = 4
        OTHERS                  = 5
        ).

    IF sy-subrc = 0.

      READ TABLE lt_file_table INDEX 1 INTO p_file.

    ELSE.

      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

  ENDMETHOD.

  METHOD get_desktop_directory.

    cl_gui_frontend_services=>get_desktop_directory(
      CHANGING
        desktop_directory    = r_desktop_directory
      EXCEPTIONS
        cntl_error           = 1
        error_no_gui         = 2
        not_supported_by_gui = 3
        OTHERS               = 4
    ).

    IF sy-subrc = 0.

      cl_gui_cfw=>flush(
        EXCEPTIONS
          cntl_system_error = 1
          cntl_error        = 2
          OTHERS            = 3
      ).

      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                   WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.
    ENDIF.

  ENDMETHOD.

  METHOD run.

    DATA lt_exclude_lines TYPE trext_parameter_value.
    FIELD-SYMBOLS <lw_exclude> LIKE LINE OF lt_exclude_lines.

    IF p_ex_par IS NOT INITIAL.
      APPEND INITIAL LINE TO lt_exclude_lines ASSIGNING <lw_exclude>.
      <lw_exclude>-parameter = p_ex_par.
      <lw_exclude>-value     = p_ex_val.
    ENDIF.

    DATA:
      lt_xml_values TYPE zcl_util_xml_analyst=>tty_xml_values.

    CREATE OBJECT go_util_xml_analyst
      EXPORTING
        i_xml_path_file = p_file
        i_exclude_lines = lt_exclude_lines.

    lt_xml_values = go_util_xml_analyst->get_values_as_table( ).

    TRY.
        cl_salv_table=>factory(
          IMPORTING
            r_salv_table   = go_alv
          CHANGING
            t_table        = lt_xml_values
        ).

        alv_config( ).

        go_alv->display( ).

      CATCH cx_salv_msg.    " ALV: General Error Class with Message
    ENDTRY.

  ENDMETHOD.

  METHOD alv_config.

    CONSTANTS:
       BEGIN OF cs_ref_ddic_text,
        medium TYPE lvc_ddict VALUE 'M' ##no_text,
        short  TYPE lvc_ddict VALUE 'S' ##no_text,
      END OF cs_ref_ddic_text.

    DATA:
       lo_column      TYPE REF TO cl_salv_column_table,
       lt_colunas     TYPE salv_t_column_ref,
       lv_short_text  TYPE scrtext_s,
       lv_medium_text TYPE scrtext_m,
       lv_list_header TYPE lvc_title  .

    lv_list_header = go_util_xml_analyst->get_root_node_name( ).

    go_alv->get_display_settings( )->set_list_header( lv_list_header ).
    go_alv->get_display_settings( )->set_striped_pattern( abap_true ).
    go_alv->get_functions( )->set_all( ).
    go_alv->get_columns( )->set_optimize( ).
    go_alv->get_columns( )->set_key_fixation( abap_true ).

    lt_colunas = go_alv->get_columns( )->get( ).
    FIELD-SYMBOLS <lw_coluna> LIKE LINE OF lt_colunas.

    LOOP AT lt_colunas ASSIGNING <lw_coluna>.
      lo_column ?= <lw_coluna>-r_column.

      CASE sy-tabix.

        WHEN 1 OR 4.

          IF sy-tabix = 1.
            lo_column->set_key( ).
            lv_short_text = text-001.
          ELSE. " 4
            lv_short_text = text-005.
          ENDIF.

          lo_column->set_short_text( lv_short_text ).
          lo_column->set_fixed_header_text( cs_ref_ddic_text-short ).

          CONTINUE.

        WHEN 2.
          lv_medium_text = text-002.

        WHEN 3.
          lv_medium_text = text-003.

        WHEN 5.
          lv_medium_text = text-004.

      ENDCASE.

      lo_column->set_fixed_header_text( cs_ref_ddic_text-medium ).
      lo_column->set_medium_text( lv_medium_text ).

    ENDLOOP.

  ENDMETHOD.

ENDCLASS.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file.

  lcl_main=>get_directory( ).

START-OF-SELECTION.

  lcl_main=>run( ).
