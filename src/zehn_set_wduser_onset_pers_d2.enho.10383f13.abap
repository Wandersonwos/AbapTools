"Name: \TY:CL_WDR_CONFIG_COPC_CONTAINER\ME:AFTER_SAVE_IMPL\SE:BEGIN\EI
ENHANCEMENT 0 ZEHN_SET_WDUSER_ONSET_PERS_D2.
DATA(lv_real_wd_user) = zcl_wda_tools=>get_real_wd_user( fixed_user = sy-uname ).

IF lv_real_wd_user IS NOT INITIAL.

  cl_wdr_cfg_persistence_utils=>config_changed(
    action              = action
    config_key          = outline_data-key
    devclass            = devclass
    environment         = environment
    is_component        = abap_true
    object_name         = outline_data-object_name
    pers_scope          = scope
    transport           = transport
    uname               = lv_real_wd_user
    config_data         = saved_comp
    source_config_key   = read_key ).
  clear saved_comp.

    RETURN.
ENDIF.


ENDENHANCEMENT.
