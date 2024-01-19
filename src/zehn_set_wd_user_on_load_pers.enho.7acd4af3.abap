"Name: \TY:CL_WD_CONFIGURATION_UTILITIES\ME:GET_PERS_LIST_4_CONFIG_ID\SE:BEGIN\EI
ENHANCEMENT 0 ZEHN_SET_WD_USER_ON_LOAD_PERS.

DATA(lv_real_wd_user) = zcl_wda_tools=>get_real_wd_user( user ).

IF lv_real_wd_user IS NOT INITIAL.

  get_pers_list_4_config_id(
    EXPORTING
      config_id         = config_id
      config_variant    = config_variant
      user              = lv_real_wd_user " <<<
      use_role          = use_role
      use_iview         = use_iview
      use_all           = use_all
      use_user          = use_user
      config_type       = config_type
      use_config_type   = use_config_type
      called_at_runtime = called_at_runtime
    RECEIVING
      pers_list         = pers_list
  ).

  RETURN.
ENDIF.

ENDENHANCEMENT.
