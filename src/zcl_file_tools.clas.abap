CLASS zcl_file_tools DEFINITION
  PUBLIC FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    TYPES: BEGIN OF ty_file_signature,
             path     TYPE string,
             filename TYPE string,
           END OF ty_file_signature,
           BEGIN OF ty_file.
             INCLUDE TYPE ty_file_signature.
    TYPES:   data TYPE xstring,
           END OF ty_file.
    TYPES ty_files_tt TYPE STANDARD TABLE OF ty_file WITH DEFAULT KEY.

    CLASS-METHODS get_mimetype_from_file_name
      IMPORTING i_file_name       TYPE clike
      RETURNING VALUE(r_mimetype) TYPE string.

    CLASS-METHODS get_mimetype_from_extension
      IMPORTING i_extension       TYPE clike
      RETURNING VALUE(r_mimetype) TYPE mimetypes-type.

    CLASS-METHODS read_binary_file_from_server
      IMPORTING i_file_path      TYPE string OPTIONAL
                i_file_name      TYPE string OPTIONAL
                i_full_file_path TYPE string OPTIONAL
      EXPORTING e_file_content   TYPE xstring
                e_error_msg      TYPE bapiret2_tab.

    CLASS-METHODS write_binary_file_from_server
      IMPORTING i_file_path      TYPE string OPTIONAL
                i_file_name      TYPE string OPTIONAL
                i_full_file_path TYPE string OPTIONAL
                i_file_content   TYPE xstring
      EXPORTING e_success        TYPE abap_bool
                e_error_msg      TYPE bapiret2.

    CLASS-METHODS transfer_file_in_server
      IMPORTING i_full_file_path_origin TYPE string
                i_full_file_path_dest   TYPE string.

    CLASS-METHODS get_extension
      IMPORTING i_file_name         TYPE clike
      RETURNING VALUE(rv_extension) TYPE mimetypes-extension.

    "!
    "! @parameter iv_dir_name |
    "! @parameter file_mask |
    "! @parameter minimium_age | Minimium age from file in seconds
    "! @parameter maximium_age |
    "! @parameter dir_name |
    "! @parameter file_counter |
    "! @parameter error_counter |
    "! @parameter files |
    "! @parameter return_code |
    CLASS-METHODS get_files_from_server
      IMPORTING iv_dir_name   TYPE eps2filnam
                file_mask     TYPE epsfilnam DEFAULT space
                minimium_age  TYPE i         OPTIONAL
                maximium_age  TYPE i         OPTIONAL
      EXPORTING dir_name      TYPE epsdirnam
                file_counter  TYPE epsfilsiz
                error_counter TYPE epsfilsiz
                files         TYPE eps2filis
                return_code   TYPE sy-subrc.

    CLASS-METHODS unzip_file
      IMPORTING iv_xstr         TYPE xstring
      RETURNING VALUE(rt_files) TYPE ty_files_tt
      RAISING   cx_t100_msg.

    CLASS-METHODS extract_filename
      IMPORTING iv_str      TYPE string
      EXPORTING ev_path     TYPE string
                ev_filename TYPE string
      RAISING   cx_t100_msg.

  PRIVATE SECTION.
    CLASS-METHODS normalize_path
      CHANGING ct_files TYPE ty_files_tt
      RAISING  cx_t100_msg.
ENDCLASS.


CLASS zcl_file_tools IMPLEMENTATION.
  METHOD get_mimetype_from_extension.
    CALL FUNCTION 'SDOK_MIMETYPE_GET'
      EXPORTING extension = i_extension
      IMPORTING mimetype  = r_mimetype.
  ENDMETHOD.

  METHOD get_mimetype_from_file_name.
    r_mimetype = get_mimetype_from_extension( get_extension( i_file_name ) ).
  ENDMETHOD.

  METHOD get_extension.
    DATA lv_dot_offset TYPE i.

    FIND FIRST OCCURRENCE OF REGEX '\.[^\.]+$' IN i_file_name MATCH OFFSET lv_dot_offset.
    lv_dot_offset = lv_dot_offset + 1.
    rv_extension = i_file_name+lv_dot_offset.
  ENDMETHOD.

  METHOD read_binary_file_from_server.
    DATA lv_content        TYPE xstring.
    DATA lv_full_file_path TYPE string.
    DATA lv_message        TYPE string.
    DATA lo_erro           TYPE REF TO cx_root.

    CLEAR:
       e_error_msg,
       e_file_content.

    IF i_full_file_path IS NOT INITIAL.
      lv_full_file_path = i_full_file_path.
    ELSE.
      lv_full_file_path = i_file_path && '\' && i_file_name.
    ENDIF.

    TRY.
        " Origem ficheiro
        TRY.
            OPEN DATASET lv_full_file_path FOR INPUT IN BINARY MODE MESSAGE lv_message.
          CATCH cx_sy_file_open.
        ENDTRY.

        IF sy-subrc = 0.
          DO.
            READ DATASET lv_full_file_path INTO lv_content.
            IF sy-subrc = 0.
              e_file_content = e_file_content && lv_content.
            ELSE.
              EXIT.
            ENDIF.
          ENDDO.

          CLOSE DATASET lv_full_file_path.

        ELSE.

          MESSAGE e000(pfm_12ca) WITH i_file_name INTO sy-tvar9. cl_msr_vrm_data=>convert_symessage2bapiret2( CHANGING ct_bapiret2 = e_error_msg ).
          "  Erro a abrir/ler ficheiro &

          MESSAGE e398(00) WITH lv_message INTO sy-tvar9. cl_msr_vrm_data=>convert_symessage2bapiret2( CHANGING ct_bapiret2 = e_error_msg ).
          "      & & & &

        ENDIF.

      CATCH cx_root INTO lo_erro.

        MESSAGE e000(pfm_12ca) WITH i_file_name INTO sy-tvar9. cl_msr_vrm_data=>convert_symessage2bapiret2( CHANGING ct_bapiret2 = e_error_msg ).
        "  Erro a abrir/ler ficheiro &

        CALL FUNCTION 'RS_EXCEPTION_TO_BAPIRET2'
          EXPORTING i_r_exception = lo_erro
          CHANGING  c_t_bapiret2  = e_error_msg.

    ENDTRY.
  ENDMETHOD.

  METHOD get_files_from_server.
    CLEAR files.

    CALL FUNCTION 'EPS2_GET_DIRECTORY_LISTING'
      EXPORTING  iv_dir_name            = iv_dir_name
                 file_mask              = file_mask
      IMPORTING  dir_name               = dir_name
                 file_counter           = file_counter
                 error_counter          = error_counter
      TABLES     dir_list               = files
      EXCEPTIONS invalid_eps_subdir     = 1
                 sapgparam_failed       = 2
                 build_directory_failed = 3
                 no_authorization       = 4
                 read_directory_failed  = 5
                 too_many_read_errors   = 6
                 empty_directory_list   = 7
                 OTHERS                 = 8.

    IF sy-subrc <> 0.
      return_code = sy-subrc.
      RETURN.
    ENDIF.

    TRY.

        IF minimium_age <= 0 AND maximium_age <= 0.
          RETURN.
        ENDIF.

        IF minimium_age > 0.

          DATA(lv_minimium_age) = minimium_age * '-1'.

          cl_abap_tstmp=>td_add( EXPORTING date     = sy-datum
                                           time     = sy-uzeit
                                           secs     = lv_minimium_age
                                 IMPORTING res_date = DATA(lv_min_limit_date)
                                           res_time = DATA(lv_min_limit_time) ).
        ENDIF.

        IF maximium_age > 0.

          DATA(lv_maximium_age) = maximium_age * '-1'.

          cl_abap_tstmp=>td_add( EXPORTING date     = sy-datum
                                           time     = sy-uzeit
                                           secs     = lv_maximium_age
                                 IMPORTING res_date = DATA(lv_max_limit_date)
                                           res_time = DATA(lv_max_limit_time) ).

        ENDIF.

        LOOP AT files REFERENCE INTO DATA(ls_dir_file).
          DATA(lv_tabix) = sy-tabix.

          DATA(lv_file_date) = CONV d( ls_dir_file->mtim+6(4)  && ls_dir_file->mtim+3(2)  && ls_dir_file->mtim(2)    ).
          DATA(lv_file_hour) = CONV t( ls_dir_file->mtim+11(2) && ls_dir_file->mtim+14(2) && ls_dir_file->mtim+17(2) ).

          IF minimium_age > 0 AND NOT zcl_date_time_tools=>check_date_time_period( start_date  = lv_file_date
                                                                                   start_time  = lv_file_hour
                                                                                   finish_date = lv_min_limit_date
                                                                                   finish_time = lv_min_limit_time
                                                                                   ignore_sec  = abap_false    ).
            DELETE files INDEX lv_tabix.
            error_counter = error_counter + 1.
          ENDIF.

          IF maximium_age > 0 AND NOT zcl_date_time_tools=>check_date_time_period( start_date  = lv_max_limit_date
                                                                                   start_time  = lv_max_limit_time
                                                                                   finish_date = lv_file_date
                                                                                   finish_time = lv_file_hour
                                                                                   ignore_sec  = abap_false    ).
            DELETE files INDEX lv_tabix.
            error_counter = error_counter + 1.
          ENDIF.

        ENDLOOP.

      CATCH cx_root.
    ENDTRY.
  ENDMETHOD.

  METHOD unzip_file.
    DATA lo_zip  TYPE REF TO cl_abap_zip.
    DATA lv_data TYPE xstring.

    FIELD-SYMBOLS <ls_zipfile> LIKE LINE OF lo_zip->files.
    FIELD-SYMBOLS <ls_file>    LIKE LINE OF rt_files.

    lo_zip = NEW #( ).
    lo_zip->load( EXPORTING  zip             = iv_xstr
                  EXCEPTIONS zip_parse_error = 1
                             OTHERS          = 2 ).
    IF sy-subrc <> 0.
      MESSAGE e001(00) WITH 'error from zip' INTO sy-tvar9. zcl_message_tools=>raise_sy_msg_as_cx_t100_msg( ).
    ENDIF.

    LOOP AT lo_zip->files ASSIGNING <ls_zipfile>.

      lo_zip->get( EXPORTING  name                    = <ls_zipfile>-name
                   IMPORTING  content                 = lv_data
                   EXCEPTIONS zip_index_error         = 1
                              zip_decompression_error = 2
                              OTHERS                  = 3 ).
      IF sy-subrc <> 0.
        MESSAGE e001(00) WITH 'error from zip get' INTO sy-tvar9. zcl_message_tools=>raise_sy_msg_as_cx_t100_msg( ).
      ENDIF.

      APPEND INITIAL LINE TO rt_files ASSIGNING <ls_file>.

      extract_filename( EXPORTING iv_str      = <ls_zipfile>-name
                        IMPORTING ev_path     = <ls_file>-path
                                  ev_filename = <ls_file>-filename ).

      <ls_file>-data = lv_data.

    ENDLOOP.

    DELETE rt_files WHERE filename IS INITIAL.

    normalize_path( CHANGING ct_files = rt_files ).
  ENDMETHOD.

  METHOD extract_filename.

    IF iv_str CA '/'.

      FIND REGEX '(.*/)(.*)' IN iv_str SUBMATCHES ev_path ev_filename.

      IF sy-subrc <> 0.
        MESSAGE e001(00) WITH 'Malformed path' INTO sy-tvar9. zcl_message_tools=>raise_sy_msg_as_cx_t100_msg( ).
      ENDIF.
      IF ev_path <> '/'.
        CONCATENATE '/' ev_path INTO ev_path.
      ENDIF.

      ev_filename = to_lower( ev_filename ).

    ELSEIF iv_str CA '\'.

      DATA lv_regex TYPE string VALUE '\b((?#drive)[a-z]):\\((?#folder)[^/:*?"<>|\r\n]*\\)?((?#file)[^\\/:*?"<>|\r\n]*)'.
      " TODO: variable is assigned but never used (ABAP cleaner)
      DATA lv_drive TYPE string.

      FIND FIRST OCCURRENCE OF REGEX lv_regex IN iv_str SUBMATCHES lv_drive ev_path ev_filename IGNORING CASE.

    ENDIF.

    IF ev_filename IS INITIAL.
      ev_path = '/'.
      ev_filename = iv_str.
    ENDIF.
  ENDMETHOD.

  METHOD normalize_path.
    " removes first folder from path if needed

    DATA lt_split  TYPE TABLE OF string.
    DATA lv_needed TYPE abap_bool.
    DATA lv_length TYPE i.
    DATA lv_split  LIKE LINE OF lt_split.

    FIELD-SYMBOLS <ls_file> LIKE LINE OF ct_files.

    READ TABLE ct_files INDEX 1 ASSIGNING <ls_file>.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    SPLIT <ls_file>-path AT '/' INTO TABLE lt_split.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.
    READ TABLE lt_split INDEX 2 INTO lv_split.
    IF sy-subrc <> 0 OR strlen( lv_split ) = 0.
      RETURN.
    ENDIF.

    CONCATENATE '/' lv_split '/*' INTO lv_split.

    lv_needed = abap_true.
    LOOP AT ct_files ASSIGNING <ls_file>.
      IF NOT <ls_file>-path CP lv_split.
        lv_needed = abap_false.
        EXIT. " current loop
      ENDIF.
    ENDLOOP.

    IF lv_needed = abap_true.
      lv_length = strlen( lv_split ) - 2.
      LOOP AT ct_files ASSIGNING <ls_file>.
        <ls_file>-path = <ls_file>-path+lv_length.
      ENDLOOP.
    ENDIF.
  ENDMETHOD.

  METHOD write_binary_file_from_server.
    CONSTANTS c_limite_caracteres TYPE i VALUE 200.

    " TODO: variable is assigned but never used (ABAP cleaner)
    DATA lv_output_length  TYPE i.
    DATA lt_data_tab       TYPE STANDARD TABLE OF rcgrepfile.
    DATA lv_full_file_path TYPE string.

    CLEAR e_error_msg.

    IF i_full_file_path IS NOT INITIAL.
      lv_full_file_path = i_full_file_path.
    ELSE.
      lv_full_file_path = i_file_path && '\' && i_file_name.
    ENDIF.

    IF strlen( lv_full_file_path ) > c_limite_caracteres.
      MESSAGE e372(zz_sigma) WITH c_limite_caracteres INTO sy-tvar9.
      " O nome do ficheiro não pode exceder &1 caracteres.
      e_error_msg = zcl_message_tools=>conv_sy_message_2_bapiret2( ).
      RETURN.
    ENDIF.

    CALL FUNCTION 'SCMS_XSTRING_TO_BINARY'
      EXPORTING buffer        = i_file_content
      IMPORTING output_length = lv_output_length
      TABLES    binary_tab    = lt_data_tab.

    DATA(lv_cprog) = sy-cprog.

    sy-cprog = 'RC1TCG3Z'.

    CALL FUNCTION 'C13Z_RAWDATA_WRITE'
      EXPORTING  i_file           = CONV eseiefile( lv_full_file_path )
                 i_file_size      = xstrlen( i_file_content )
                 i_lines          = lines( lt_data_tab )
      TABLES     i_rcgrepfile_tab = lt_data_tab
      EXCEPTIONS no_permission    = 1
                 open_failed      = 2
                 ap_file_exists   = 3
                 close_failed     = 4
                 write_failed     = 5
                 OTHERS           = 6.

    DATA(lv_sy_subrc) = sy-subrc.

    sy-cprog = lv_cprog.

    IF lv_sy_subrc = 0.
      e_success = abap_true.
    ELSE.
      e_error_msg = zcl_message_tools=>conv_sy_message_2_bapiret2( ).
    ENDIF.
  ENDMETHOD.

  METHOD transfer_file_in_server.
*      zcl_file_tools=>read_binary_file_from_server(
*        EXPORTING
*          i_file_path    = CONV #( lv_diretoria_pdf_ler )
*          i_file_name    = CONV #( ls_pdf->name )
*        IMPORTING
*          e_file_content = DATA(ls_file_content)
**         e_error_msg    =
*      ).
*
*      zcl_file_tools=>write_binary_file_from_server(
*        EXPORTING
*          i_file_path    = CONV #( lv_diretoria_pdf_ler )
*          i_file_name    = CONV #( ls_pdf->name )
*          i_file_content = ls_file_content
*      ).

*call FUNCTION 'ARCHIVFILE_SERVER_TO_SERVER'
*  EXPORTING
*    sourcepath       = conv saepfad( lv_diretoria_pdf_ler      && ls_pdf->name )
*    targetpath       = conv saepfad( lv_diretoria_pdf_escrever && ls_pdf->name )
**  IMPORTING
**    length           =
**  EXCEPTIONS
**    error_file       = 1
**    no_authorization = 2
**    others           = 3
*  .
*IF sy-subrc = 0.
*data(lv_file) = conv saepfad( lv_diretoria_pdf_ler      && ls_pdf->name ).
*DELETE DATASET lv_file.
*ENDIF.

    DATA l_stat TYPE c LENGTH 1.

    CALL FUNCTION 'SXPG_COMMAND_EXECUTE'
      EXPORTING
        commandname                   = 'Z_MOVE_FILE'
        additional_parameters         = CONV btcxpgpar( |{ i_full_file_path_origin } { i_full_file_path_dest }| )
      IMPORTING
        status                        = l_stat
      EXCEPTIONS
        no_permission                 = 1
        command_not_found             = 2
        parameters_too_long           = 3
        security_risk                 = 4
        wrong_check_call_interface    = 5
        program_start_error           = 6
        program_termination_error     = 7
        x_error                       = 8
        parameter_expected            = 9
        too_many_parameters           = 10
        illegal_command               = 11
        wrong_asynchronous_parameters = 12
        cant_enq_tbtco_entry          = 13
        jobcount_generation_error     = 14
        OTHERS                        = 15.

    IF sy-subrc = 0 AND l_stat = 'o'.

    ENDIF.
  ENDMETHOD.
ENDCLASS.
