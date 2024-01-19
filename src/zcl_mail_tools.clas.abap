CLASS zcl_mail_tools DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    CLASS-METHODS:
      convert_so10_text_in_html
        IMPORTING
          body_text_name        TYPE thead-tdname
          itf_text              TYPE tline_tab
          first_line_is_subject TYPE abap_bool OPTIONAL
          tokens_values         TYPE fpmgb_t_namevalue OPTIONAL
        EXPORTING
          subject               TYPE so_obj_des
          html_text             TYPE soli_tab,
      replace_token_values
        IMPORTING
          tokens_values TYPE fpmgb_t_namevalue
        CHANGING
          text          TYPE STANDARD TABLE.

  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_mail_tools IMPLEMENTATION.
  METHOD convert_so10_text_in_html.

    DATA lt_html_text TYPE htmltable.

    CHECK itf_text IS NOT INITIAL.

    CLEAR:
      subject,
      html_text.

    DATA(lt_ift_text) = itf_text.

    IF first_line_is_subject = abap_true.
      subject = lt_ift_text[ 1 ]-tdline.
      DELETE lt_ift_text INDEX 1.

      LOOP AT tokens_values REFERENCE INTO DATA(ls_token_value).

        REPLACE ALL OCCURRENCES OF ls_token_value->name
                                IN subject
                              WITH ls_token_value->value.
      ENDLOOP.
    ENDIF.

    CALL FUNCTION 'CONVERT_ITF_TO_HTML'
      EXPORTING
        i_header       = VALUE thead( tdid     = cl_hriq_enter_grades_constant=>c_appr_tdid
                                      tdname   = body_text_name
                                      tdobject = cl_cocf_shift_note=>con_default_texts
                                      tdspras  = sy-langu                                   )
      TABLES
        t_itf_text     = lt_ift_text
        t_html_text    = lt_html_text
      EXCEPTIONS
        syntax_check   = 1
        replace        = 2
        illegal_header = 3
        OTHERS         = 4.

    html_text = lt_html_text.

    replace_token_values(
      EXPORTING
        tokens_values = tokens_values
      CHANGING
        text          = html_text
    ).

  ENDMETHOD.

  METHOD replace_token_values.

    DATA lv_linha_str TYPE string.

    LOOP AT text ASSIGNING FIELD-SYMBOL(<ls_text>).
      DATA(lv_tabix) = sy-tabix.

      LOOP AT tokens_values REFERENCE INTO DATA(ls_token_value).

        lv_linha_str = <ls_text>.
        REPLACE ALL OCCURRENCES OF ls_token_value->name
                                IN lv_linha_str
                              WITH ls_token_value->value.
        CHECK sy-subrc = 0.
        IF strlen( lv_linha_str ) <= 255.
          <ls_text> = lv_linha_str.
        ELSE.
          <ls_text> = lv_linha_str(255).
          INSERT lv_linha_str+255 INTO text INDEX lv_tabix + 1.
        ENDIF.
      ENDLOOP.
    ENDLOOP.

  ENDMETHOD.

ENDCLASS.
