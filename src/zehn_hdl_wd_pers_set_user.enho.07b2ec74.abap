"Name: \TY:CL_WD_PERS_FACTORY\ME:GET_COMP_PERS_API\SE:BEGIN\EI
ENHANCEMENT 0 ZEHN_HDL_WD_PERS_SET_USER.

DATA(lv_real_wd_user) = zcl_wda_tools=>get_real_wd_user( user ).

IF lv_real_wd_user IS NOT INITIAL.

  api = get_comp_pers_api(
          EXPORTING
            config_key            = config_key
            user                  = lv_real_wd_user
            is_webdynpro_env      = abap_true
            md_provider           = md_provider
            with_default_var_info = with_default_var_info
  ).

  RETURN.
ENDIF.

ENDENHANCEMENT.
