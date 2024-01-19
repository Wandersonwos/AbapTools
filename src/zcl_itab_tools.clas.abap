CLASS zcl_itab_tools DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    CLASS-METHODS:
      create_xlsx_in_xstring
        IMPORTING
          it_fieldcat      TYPE lvc_t_fcat OPTIONAL
          it_sort          TYPE lvc_t_sort OPTIONAL
          it_filt          TYPE lvc_t_filt OPTIONAL
          is_layout        TYPE lvc_s_layo OPTIONAL
          it_hyperlinks    TYPE lvc_t_hype OPTIONAL
          VALUE(itdata)    TYPE STANDARD TABLE
        RETURNING
          VALUE(r_xstring) TYPE xstring,
      create_fieldcat_using_salv
        EXPORTING
          VALUE(r_result) TYPE lvc_t_fcat
        CHANGING
          itdata          TYPE STANDARD TABLE,
      create_fieldcat_using_rtti
        IMPORTING
          pt_table           TYPE STANDARD TABLE
        RETURNING
          VALUE(pt_fieldcat) TYPE lvc_t_fcat,
      move_list_item
        IMPORTING
          ok_code  TYPE sy-ucomm
        CHANGING
          ct_table TYPE STANDARD TABLE.

  PRIVATE SECTION.
*    CLASS-DATA: lt_data    TYPE REF TO data.

ENDCLASS.



CLASS zcl_itab_tools IMPLEMENTATION.


  METHOD create_fieldcat_using_rtti.

    DATA:
      lr_tabdescr TYPE REF TO cl_abap_structdescr,
      lr_data     TYPE REF TO data,
      lt_dfies    TYPE ddfields,
      ls_dfies    TYPE dfies,
      ls_fieldcat TYPE lvc_s_fcat.

    CLEAR pt_fieldcat.
    CREATE DATA lr_data LIKE LINE OF pt_table.
    lr_tabdescr ?= cl_abap_structdescr=>describe_by_data_ref( lr_data ).
    lt_dfies = cl_salv_data_descr=>read_structdescr( lr_tabdescr ).

    LOOP AT lt_dfies
    INTO    ls_dfies.
      CLEAR ls_fieldcat.
      MOVE-CORRESPONDING ls_dfies TO ls_fieldcat.
      APPEND ls_fieldcat TO pt_fieldcat.
    ENDLOOP.

  ENDMETHOD.


  METHOD create_fieldcat_using_salv.

    DATA:
      lt_data    TYPE REF TO data,
      salv_table TYPE REF TO cl_salv_table,
      lt_fcat    TYPE lvc_t_fcat.

    GET REFERENCE OF itdata INTO lt_data.

    FIELD-SYMBOLS: <tab> TYPE STANDARD TABLE.
    ASSIGN lt_data->* TO <tab>.
    TRY.
        cl_salv_table=>factory(
        EXPORTING
          list_display = abap_false
        IMPORTING
          r_salv_table = salv_table
        CHANGING
          t_table      = <tab> ).

        lt_fcat = cl_salv_controller_metadata=>get_lvc_fieldcatalog(
                                 r_columns      = salv_table->get_columns( )
                                 r_aggregations = salv_table->get_aggregations( ) ).
      CATCH cx_salv_msg.
        RETURN.
    ENDTRY.

  ENDMETHOD.


  METHOD create_xlsx_in_xstring.

    DATA:
      lt_data TYPE REF TO data,
      lt_fcat TYPE lvc_t_fcat.

    GET REFERENCE OF itdata INTO lt_data.

    IF it_fieldcat IS INITIAL.
      lt_fcat = create_fieldcat_using_rtti( itdata ).
    ELSE.
      lt_fcat = it_fieldcat.
    ENDIF.

    cl_salv_bs_lex=>export_from_result_data_table(
      EXPORTING
        is_format            = if_salv_bs_lex_format=>mc_format_xlsx
        ir_result_data_table = cl_salv_ex_util=>factory_result_data_table(
                                                r_data         = lt_data
                                                s_layout       = is_layout
                                                t_fieldcatalog = lt_fcat
                                                t_sort         = it_sort
                                                t_filter       = it_filt
                                                t_hyperlinks   = it_hyperlinks )
      IMPORTING
        er_result_file       = r_xstring ).

  ENDMETHOD.
  METHOD move_list_item.

    FIELD-SYMBOLS <ls_line_ref_old> TYPE any.

    DATA:
      lv_tabix_new      TYPE i,
      lt_selected_lines TYPE rstabixtab,
      ls_line_ref       TYPE REF TO data.

    LOOP AT ct_table ASSIGNING FIELD-SYMBOL(<ls_line>).
      DATA(lv_tabix) = sy-tabix.
      ASSIGN COMPONENT if_eamprt_const=>c_ucomm_mark OF STRUCTURE <ls_line> TO FIELD-SYMBOL(<lv_field_mark>).
      CHECK sy-subrc = 0 AND <lv_field_mark> = abap_true.
      APPEND VALUE #( tabix = lv_tabix ) TO lt_selected_lines.
    ENDLOOP.

    DATA(lv_qtd_lines)          = lines( ct_table         ).
    DATA(lv_qtd_lines_selected) = lines( lt_selected_lines ).

    IF lv_qtd_lines_selected <> 1.

      MESSAGE s218(idcn) WITH lv_qtd_lines_selected DISPLAY LIKE if_cwd_constants=>c_message_error.
      " Selecione somente uma linha; você selecionou &1 linhas
      RETURN.
    ENDIF.

    DATA(lv_tabix_old) = lt_selected_lines[ 1 ]-tabix.

* Aborta nos casos em que a linha não deve ser movida
    IF ( lv_tabix_old = 1 AND ( ok_code CP '*UP_ROW' OR
                                ok_code CP '*TOP_ROW'   ) )
       OR
       ( lv_tabix_old = lv_qtd_lines AND ( ok_code CP '*DOWN_ROW' OR
                                           ok_code CP '*BOTTON_ROW' ) ).
      RETURN.
    ENDIF.

* Determina qual será a nova posição da linha
    lv_tabix_new = COND #( WHEN ok_code CP '*UP_ROW'     THEN lv_tabix_old - 1
                           WHEN ok_code CP '*DOWN_ROW'   THEN lv_tabix_old + 1
                           WHEN ok_code CP '*TOP_ROW'    THEN 1
                           WHEN ok_code CP '*BOTTON_ROW' THEN lv_qtd_lines    ).

* Move a linha selecionada para a nova posição
    CREATE DATA ls_line_ref LIKE LINE OF ct_table.

    ASSIGN ls_line_ref->* TO FIELD-SYMBOL(<ls_line_ref>).

    ASSIGN ct_table[ lv_tabix_old ] TO <ls_line_ref_old>.
    <ls_line_ref> = <ls_line_ref_old>.

    DELETE ct_table INDEX lv_tabix_old.
    INSERT <ls_line_ref> INTO ct_table INDEX lv_tabix_new.

* Atualiza a linha selecionada de acordo com a nova posição
    LOOP AT ct_table ASSIGNING <ls_line>.
      lv_tabix = sy-tabix.

      ASSIGN COMPONENT if_eamprt_const=>c_ucomm_mark OF STRUCTURE <ls_line> TO <lv_field_mark>.
      CHECK sy-subrc = 0.
      <lv_field_mark> = xsdbool( lv_tabix = lv_tabix_new ).

    ENDLOOP.

  ENDMETHOD.

ENDCLASS.
