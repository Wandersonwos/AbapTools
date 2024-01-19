"Name: \TY:CL_WD_CONFIGURATION_UTILITIES\ME:GET_PERS_LIST_4_DEFAULT_VAR\SE:BEGIN\EI
ENHANCEMENT 0 ZEHN_SET_WD_USER_ON_LOAD_PERS.
DATA(lv_real_wd_user) = zcl_wda_tools=>get_real_wd_user( fixed_user = user ).

IF lv_real_wd_user IS NOT INITIAL.
  get_pers_list_4_default_var(
    EXPORTING
      config_key                  = config_key
      ignore_user_personalization = ignore_user_personalization
      user                        = lv_real_wd_user
    IMPORTING
      default_variant             = default_variant
      user_type                   = user_type
  ).
  RETURN.
ENDIF.

ENDENHANCEMENT.
