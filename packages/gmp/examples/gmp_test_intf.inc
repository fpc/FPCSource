{ Note: This doesn't test GMP per se.

  Not covered:
    mp_printf
    mp_scanf
    mpz_array_init
    errno
}

{$assertions on - keep this}

type

  { TTestGmpBinding }

  TTestGmpBinding = class(TTestCase)
  published
    procedure mp_get_memory_functions_test;
    procedure mp_set_memory_functions_test;

    procedure mp_randinit_default_test;
    procedure mp_randinit_mt_test;
    procedure mp_randinit_lc_2exp_test;
    procedure mp_randinit_lc_2exp_size_test;
    procedure mp_randinit_set_test;
    procedure mp_randclear_test;
    procedure mp_randseed_test;
    procedure mp_randseed_ui_test;
    procedure mp_urandomb_ui_test;
    procedure mp_urandomm_ui_test;

    procedure mp_asprintf_test;
    procedure mp_asprintf_test2;
    procedure mp_snprintf_test;
    procedure mp_snprintf_test2;
    procedure mp_sprintf_test;
    procedure mp_sprintf_test2;
    procedure mp_sscanf_test;
    procedure mp_sscanf_test2;

    procedure mpz_init_test;
    procedure mpz_clear_test;
    procedure mpz_init2_test;
    procedure mpz_init_set_si_test;
    procedure mpz_init_set_ui_test;
    procedure mpz_init_set_d_test;
    procedure mpz_init_set_test;
    procedure mpz_init_set_str_test;
    procedure mpz_realloc_test;
    procedure mpz_realloc2_test;
    procedure mpz_getlimbn_test;
    procedure mpz_export_test;
    procedure mpz_import_test;
    procedure mpz_swap_test;
    procedure mpz_set_test;
    procedure mpz_set_d_test;
    procedure mpz_set_f_test;
    procedure mpz_set_q_test;
    procedure mpz_set_si_test;
    procedure mpz_set_str_test;
    procedure mpz_set_ui_test;
    procedure mpz_get_d_test;
    procedure mpz_get_d_2exp_test;
    procedure mpz_get_si_test;
    procedure mpz_get_str_test;
    procedure mpz_get_ui_test;
    procedure mpz_add_test;
    procedure mpz_add_ui_test;
    procedure mpz_sub_test;
    procedure mpz_sub_ui_test;
    procedure mpz_ui_sub_test;
    procedure mpz_mul_test;
    procedure mpz_mul_si_test;
    procedure mpz_mul_ui_test;
    procedure mpz_addmul_test;
    procedure mpz_addmul_ui_test;
    procedure mpz_submul_test;
    procedure mpz_submul_ui_test;
    procedure mpz_mul_2exp_test;
    procedure mpz_neg_test;
    procedure mpz_abs_test;

    procedure mpz_cdiv_q_test;
    procedure mpz_fdiv_q_test;
    procedure mpz_tdiv_q_test;
    procedure mpz_cdiv_q_2exp_test;
    procedure mpz_fdiv_q_2exp_test;
    procedure mpz_tdiv_q_2exp_test;
    procedure mpz_cdiv_q_ui_test;
    procedure mpz_fdiv_q_ui_test;
    procedure mpz_tdiv_q_ui_test;
    procedure mpz_cdiv_qr_test;
    procedure mpz_fdiv_qr_test;
    procedure mpz_tdiv_qr_test;
    procedure mpz_cdiv_qr_ui_test;
    procedure mpz_fdiv_qr_ui_test;
    procedure mpz_tdiv_qr_ui_test;
    procedure mpz_cdiv_r_test;
    procedure mpz_fdiv_r_test;
    procedure mpz_tdiv_r_test;
    procedure mpz_cdiv_r_2exp_test;
    procedure mpz_fdiv_r_2exp_test;
    procedure mpz_tdiv_r_2exp_test;
    procedure mpz_cdiv_r_ui_test;
    procedure mpz_fdiv_r_ui_test;
    procedure mpz_tdiv_r_ui_test;
    procedure mpz_cdiv_ui_test;
    procedure mpz_fdiv_ui_test;
    procedure mpz_tdiv_ui_test;

    procedure mpz_mod_test;
    procedure mpz_divexact_test;
    procedure mpz_divexact_ui_test;
    procedure mpz_divisible_p_test;
    procedure mpz_divisible_ui_p_test;
    procedure mpz_divisible_2exp_p_test;
    procedure mpz_congruent_p_test;
    procedure mpz_congruent_2exp_p_test;
    procedure mpz_congruent_ui_p_test;
    procedure mpz_powm_test;
    procedure mpz_powm_ui_test;
    procedure mpz_pow_ui_test;
    procedure mpz_ui_pow_ui_test;
    procedure mpz_root_test;
    procedure mpz_rootrem_test;
    procedure mpz_sqrt_test;
    procedure mpz_sqrtrem_test;
    procedure mpz_perfect_power_p_test;
    procedure mpz_perfect_square_p_test;

    procedure mpz_probab_prime_p_test;
    procedure mpz_nextprime_test;
    procedure mpz_gcd_test;
    procedure mpz_gcd_ui_test;
    procedure mpz_gcdext_test;
    procedure mpz_lcm_test;
    procedure mpz_lcm_ui_test;
    procedure mpz_invert_test;
    procedure mpz_jacobi_test;
    procedure mpz_kronecker_si_test;
    procedure mpz_kronecker_ui_test;
    procedure mpz_si_kronecker_test;
    procedure mpz_ui_kronecker_test;
    procedure mpz_remove_test;
    procedure mpz_fac_ui_test;
    procedure mpz_bin_ui_test;
    procedure mpz_bin_uiui_test;
    procedure mpz_fib_ui_test;
    procedure mpz_fib2_ui_test;
    procedure mpz_lucnum_ui_test;
    procedure mpz_lucnum2_ui_test;

    procedure mpz_cmp_test;
    procedure mpz_cmp_d_test;
    procedure mpz_cmp_si_test;
    procedure mpz_cmp_ui_test;
    procedure mpz_cmpabs_test;
    procedure mpz_cmpabs_d_test;
    procedure mpz_cmpabs_ui_test;

    procedure mpz_and_test;
    procedure mpz_ior_test;
    procedure mpz_xor_test;
    procedure mpz_com_test;
    procedure mpz_popcount_test;
    procedure mpz_hamdist_test;
    procedure mpz_scan0_test;
    procedure mpz_scan1_test;
    procedure mpz_setbit_test;
    procedure mpz_clrbit_test;
    procedure mpz_combit_test;
    procedure mpz_tstbit_test;

    procedure mpz_urandomb_test;
    procedure mpz_urandomm_test;
    procedure mpz_rrandomb_test;

    procedure mpz_fits_ushort_p_test;
    procedure mpz_fits_sshort_p_test;
    procedure mpz_fits_uint_p_test;
    procedure mpz_fits_sint_p_test;
    procedure mpz_fits_ulong_p_test;
    procedure mpz_fits_slong_p_test;
    procedure mpz_sizeinbase_test;
    procedure mpz_size_test;

    procedure mpq_init_test;
    procedure mpq_clear_test;
    procedure mpq_set_ui_test;
    procedure mpq_set_si_test;
    procedure mpq_canonicalize_test;
    procedure mpq_set_test;
    procedure mpq_setmpz_test;
    procedure mpq_set_str_test;
    procedure mpq_swap_test;

    procedure mpq_get_d_test;
    procedure mpq_set_d_test;
    procedure mpq_set_f_test;
    procedure mpq_get_str_test;

    procedure mpq_add_test;
    procedure mpq_sub_test;
    procedure mpq_mul_test;
    procedure mpq_mul_2exp_test;
    procedure mpq_div_test;
    procedure mpq_div_2exp_test;
    procedure mpq_neg_test;
    procedure mpq_abs_test;
    procedure mpq_inv_test;

    procedure mpq_cmp_test;
    procedure mpq_cmp_si_test;
    procedure mpq_cmp_ui_test;
    procedure mpq_equal_test;

    procedure mpq_get_num_test;
    procedure mpq_get_den_test;
    procedure mpq_set_num_test;
    procedure mpq_set_den_test;

    procedure mpf_get_default_prec_test;
    procedure mpf_set_default_prec_test;

    procedure mpf_init_test;
    procedure mpf_init2_test;
    procedure mpf_clear_test;
    procedure mpf_get_prec_test;
    procedure mpf_set_prec_test;
    procedure mpf_set_prec_raw_test;

    procedure mpf_set_q_test;
    procedure mpf_set_ui_test;
    procedure mpf_get_ui_test;
    procedure mpf_set_si_test;
    procedure mpf_get_si_test;
    procedure mpf_set_test;
    procedure mpf_set_str_test;
    procedure mpf_swap_test;

    procedure mpf_get_d_test;
    procedure mpf_set_d_test;
    procedure mpf_setmpz_test;
    procedure mpf_get_d_2exp_test;
    procedure mpf_get_str_test;

    procedure mpf_add_test;
    procedure mpf_add_ui_test;
    procedure mpf_sub_test;
    procedure mpf_sub_ui_test;
    procedure mpf_ui_sub_test;
    procedure mpf_mul_test;
    procedure mpf_mul_ui_test;
    procedure mpf_mul_2exp_test;
    procedure mpf_div_test;
    procedure mpf_div_ui_test;
    procedure mpf_ui_div_test;
    procedure mpf_div_2exp_test;
    procedure mpf_sqrt_test;
    procedure mpf_sqrt_ui_test;
    procedure mpf_pow_ui_test;
    procedure mpf_neg_test;
    procedure mpf_abs_test;

    procedure mpf_cmp_test;
    procedure mpf_cmp_d_test;
    procedure mpf_cmp_si_test;
    procedure mpf_cmp_ui_test;
    procedure mpf_eq_test;
    procedure mpf_reldiff_test;

    procedure mpf_ceil_test;
    procedure mpf_floor_test;
    procedure mpf_trunc_test;
    procedure mpf_integer_p_test;

    procedure mpf_fits_ushort_p_test;
    procedure mpf_fits_sshort_p_test;
    procedure mpf_fits_uint_p_test;
    procedure mpf_fits_sint_p_test;
    procedure mpf_fits_ulong_p_test;
    procedure mpf_fits_slong_p_test;

    procedure mpf_urandomb_test;
    procedure mpf_random2_test;

    procedure mpf_init_set_test;
    procedure mpf_init_set_d_test;
    procedure mpf_init_set_si_test;
    procedure mpf_init_set_str_test;
    procedure mpf_init_set_ui_test;

    procedure mpn_add_n_test;
    procedure mpn_add_1_test;
    procedure mpn_add_test;
    procedure mpn_sub_n_test;
    procedure mpn_sub_1_test;
    procedure mpn_sub_test;
    procedure mpn_mul_n_test;
    procedure mpn_mul_1_test;
    procedure mpn_mul_test;
    procedure mpn_addmul_1_test;
    procedure mpn_submul_1_test;
    procedure mpn_tdiv_qr_test;
    procedure mpn_divrem_1_test;
    procedure mpn_divexact_by3c_test;
    procedure mpn_mod_1_test;
    procedure mpn_lshift_test;
    procedure mpn_rshift_test;
    procedure mpn_cmp_test;
    procedure mpn_gcd_test;
    procedure mpn_gcd_1_test;
    procedure mpn_gcdext_test;
    procedure mpn_sqrtrem_test;
    procedure mpn_get_str_test;
    procedure mpn_set_str_test;
    procedure mpn_scan0_test;
    procedure mpn_scan1_test;
    procedure mpn_popcount_test;
    procedure mpn_hamdist_test;
    procedure mpn_perfect_square_p_test;

    procedure mpn_random_test;
    procedure mpn_random2_test;

    procedure bits_per_limb_test;
    procedure version_test;

  end;

  { TTestGmpExtensions }

  TTestGmpExtensions = class(TTestCase)
  published

    procedure randinit_default_test;
    procedure randinit_mt_test;
    procedure randinit_lc_2exp_test;
    procedure randinit_lc_2exp_size_test;
    procedure randinit_set_test;
    procedure randclear_test;
    procedure randseed_test;
    procedure randseed_ui_test;
    procedure urandomb_ui_test;
    procedure urandomm_ui_test;

    procedure z_init_test;
    procedure z_clear_test;
    procedure z_init2_test;
    procedure z_init_set_si_test;
    procedure z_init_set_ui_test;
    procedure z_init_set_d_test;
    procedure z_init_set_test;
    procedure z_init_set_str_test;
    procedure z_realloc_test;
    procedure z_realloc2_test;
    procedure z_getlimbn_test;
    procedure z_export_test;
    procedure z_import_test;
    procedure z_swap_test;
    procedure z_set_test;
    procedure z_set_d_test;
    procedure z_set_f_test;
    procedure z_set_q_test;
    procedure z_set_si_test;
    procedure z_set_str_test;
    procedure z_set_ui_test;
    procedure z_get_d_test;
    procedure z_get_d_2exp_test;
    procedure z_get_si_test;
    procedure z_get_str_test;
    procedure z_get_ui_test;
    procedure z_add_test;
    procedure z_add_ui_test;
    procedure z_sub_test;
    procedure z_sub_ui_test;
    procedure z_ui_sub_test;
    procedure z_mul_test;
    procedure z_mul_si_test;
    procedure z_mul_ui_test;
    procedure z_addmul_test;
    procedure z_addmul_ui_test;
    procedure z_submul_test;
    procedure z_submul_ui_test;
    procedure z_mul_2exp_test;
    procedure z_neg_test;
    procedure z_abs_test;

    procedure z_cdiv_q_test;
    procedure z_fdiv_q_test;
    procedure z_tdiv_q_test;
    procedure z_cdiv_q_2exp_test;
    procedure z_fdiv_q_2exp_test;
    procedure z_tdiv_q_2exp_test;
    procedure z_cdiv_q_ui_test;
    procedure z_fdiv_q_ui_test;
    procedure z_tdiv_q_ui_test;
    procedure z_cdiv_qr_test;
    procedure z_fdiv_qr_test;
    procedure z_tdiv_qr_test;
    procedure z_cdiv_qr_ui_test;
    procedure z_fdiv_qr_ui_test;
    procedure z_tdiv_qr_ui_test;
    procedure z_cdiv_r_test;
    procedure z_fdiv_r_test;
    procedure z_tdiv_r_test;
    procedure z_cdiv_r_2exp_test;
    procedure z_fdiv_r_2exp_test;
    procedure z_tdiv_r_2exp_test;
    procedure z_cdiv_r_ui_test;
    procedure z_fdiv_r_ui_test;
    procedure z_tdiv_r_ui_test;
    procedure z_cdiv_ui_test;
    procedure z_fdiv_ui_test;
    procedure z_tdiv_ui_test;

    procedure z_mod_test;
    procedure z_divexact_test;
    procedure z_divexact_ui_test;
    procedure z_divisible_p_test;
    procedure z_divisible_ui_p_test;
    procedure z_divisible_2exp_p_test;
    procedure z_congruent_p_test;
    procedure z_congruent_2exp_p_test;
    procedure z_congruent_ui_p_test;
    procedure z_powm_test;
    procedure z_powm_ui_test;
    procedure z_pow_ui_test;
    procedure z_ui_pow_ui_test;
    procedure z_root_test;
    procedure z_rootrem_test;
    procedure z_sqrt_test;
    procedure z_sqrtrem_test;
    procedure z_perfect_power_p_test;
    procedure z_perfect_square_p_test;

    procedure z_probab_prime_p_test;
    procedure z_nextprime_test;
    procedure z_gcd_test;
    procedure z_gcd_ui_test;
    procedure z_gcdext_test;
    procedure z_lcm_test;
    procedure z_lcm_ui_test;
    procedure z_invert_test;
    procedure z_jacobi_test;
    procedure z_kronecker_si_test;
    procedure z_kronecker_ui_test;
    procedure z_si_kronecker_test;
    procedure z_ui_kronecker_test;
    procedure z_remove_test;
    procedure z_fac_ui_test;
    procedure z_bin_ui_test;
    procedure z_bin_uiui_test;
    procedure z_fib_ui_test;
    procedure z_fib2_ui_test;
    procedure z_lucnum_ui_test;
    procedure z_lucnum2_ui_test;

    procedure z_cmp_test;
    procedure z_cmp_d_test;
    procedure z_cmp_si_test;
    procedure z_cmp_ui_test;
    procedure z_cmpabs_test;
    procedure z_cmpabs_d_test;
    procedure z_cmpabs_ui_test;

    procedure z_and_test;
    procedure z_ior_test;
    procedure z_xor_test;
    procedure z_com_test;
    procedure z_popcount_test;
    procedure z_hamdist_test;
    procedure z_scan0_test;
    procedure z_scan1_test;
    procedure z_setbit_test;
    procedure z_clrbit_test;
    procedure z_combit_test;
    procedure z_tstbit_test;

    procedure z_urandomb_test;
    procedure z_urandomm_test;
    procedure z_rrandomb_test;

    procedure z_fits_ushort_p_test;
    procedure z_fits_sshort_p_test;
    procedure z_fits_uint_p_test;
    procedure z_fits_sint_p_test;
    procedure z_fits_ulong_p_test;
    procedure z_fits_slong_p_test;
    procedure z_sizeinbase_test;
    procedure z_size_test;

    procedure q_init_test;
    procedure q_clear_test;
    procedure q_set_ui_test;
    procedure q_set_si_test;
    procedure q_canonicalize_test;
    procedure q_set_test;
    procedure q_set_z_test;
    procedure q_set_str_test;
    procedure q_swap_test;

    procedure q_get_d_test;
    procedure q_set_d_test;
    procedure q_set_f_test;
    procedure q_get_str_test;

    procedure q_add_test;
    procedure q_sub_test;
    procedure q_mul_test;
    procedure q_mul_2exp_test;
    procedure q_div_test;
    procedure q_div_2exp_test;
    procedure q_neg_test;
    procedure q_abs_test;
    procedure q_inv_test;

    procedure q_cmp_test;
    procedure q_cmp_si_test;
    procedure q_cmp_ui_test;
    procedure q_equal_test;

    procedure q_get_num_test;
    procedure q_get_den_test;
    procedure q_set_num_test;
    procedure q_set_den_test;

    procedure f_get_default_prec_test;
    procedure f_set_default_prec_test;

    procedure f_init_test;
    procedure f_init2_test;
    procedure f_clear_test;
    procedure f_get_prec_test;
    procedure f_set_prec_test;
    procedure f_set_prec_raw_test;

    procedure f_set_q_test;
    procedure f_set_ui_test;
    procedure f_get_ui_test;
    procedure f_set_si_test;
    procedure f_get_si_test;
    procedure f_set_test;
    procedure f_set_str_test;
    procedure f_swap_test;

    procedure f_get_d_test;
    procedure f_set_d_test;
    procedure f_set_z_test;
    procedure f_get_d_2exp_test;
    procedure f_get_str_test;

    procedure f_add_test;
    procedure f_add_ui_test;
    procedure f_sub_test;
    procedure f_sub_ui_test;
    procedure f_ui_sub_test;
    procedure f_mul_test;
    procedure f_mul_ui_test;
    procedure f_mul_2exp_test;
    procedure f_div_test;
    procedure f_div_ui_test;
    procedure f_ui_div_test;
    procedure f_div_2exp_test;
    procedure f_sqrt_test;
    procedure f_sqrt_ui_test;
    procedure f_pow_ui_test;
    procedure f_neg_test;
    procedure f_abs_test;

    procedure f_cmp_test;
    procedure f_cmp_d_test;
    procedure f_cmp_si_test;
    procedure f_cmp_ui_test;
    procedure f_eq_test;
    procedure f_reldiff_test;

    procedure f_ceil_test;
    procedure f_floor_test;
    procedure f_trunc_test;
    procedure f_integer_p_test;

    procedure f_fits_ushort_p_test;
    procedure f_fits_sshort_p_test;
    procedure f_fits_uint_p_test;
    procedure f_fits_sint_p_test;
    procedure f_fits_ulong_p_test;
    procedure f_fits_slong_p_test;

    procedure f_urandomb_test;
    procedure f_random2_test;

    procedure f_init_set_test;
    procedure f_init_set_d_test;
    procedure f_init_set_si_test;
    procedure f_init_set_str_test;
    procedure f_init_set_ui_test;

  end;

  { TTestGmpOperators }

  TTestGmpOperators = class(TTestCase)
    procedure star__MPFloat__MPFloat__MPFloat_;
    procedure star__MPInteger__MPInteger__MPInteger_;
    procedure star__MPRational__MPRational__MPRational_;
    procedure pow__MPFloat_valuint_MPFloat_;
    procedure pow__MPInteger_valuint_MPInteger_;
    procedure plus__MPFloat__MPFloat__MPFloat_;
    procedure plus__MPInteger__MPInteger__MPInteger_;
    procedure plus__MPRational__MPRational__MPRational_;
    procedure minus__MPFloat__MPFloat_;
    procedure minus__MPInteger__MPInteger_;
    procedure minus__MPRational__MPRational_;
    procedure minus__MPFloat__MPFloat__MPFloat_;
    procedure minus__MPInteger__MPInteger__MPInteger_;
    procedure minus__MPRational__MPRational__MPRational_;
    procedure slash__MPFloat__MPFloat__MPFloat_;
    procedure slash__MPInteger__MPInteger__MPInteger_;
    procedure slash__MPRational__MPRational__MPRational_;
    procedure assign_double_MPFloat_;
    procedure assign_double_MPInteger_;
    procedure assign_double_MPRational_;
    procedure assign__MPFloat_Cardinal;
    procedure assign__MPFloat_double;
    procedure assign__MPFloat_integer;
    procedure assign__MPFloat_mpf_t;
    procedure assign__MPFloat__MPInteger_;
    procedure assign__MPFloat__MPRational_;
    procedure assign__MPFloat_string;
    procedure assign__MPFloat_valsint;
    procedure assign__MPFloat_valuint;
    procedure assign_mpf_t_MPFloat_;
    procedure assign__MPInteger_cardinal;
    procedure assign__MPInteger_double;
    procedure assign__MPInteger_integer;
    procedure assign__MPInteger__MPFloat_;
    procedure assign__MPInteger__MPRational_;
    procedure assign__MPInteger_mpz_t;
    procedure assign__MPInteger_string;
    procedure assign__MPInteger_valsint;
    procedure assign__MPInteger_valuint;
    procedure assign_mpq_t_MPRational_;
    procedure assign__MPRandState_randstate_t;
    procedure assign__MPRational_double;
    procedure assign__MPRational__MPFloat_;
    procedure assign__MPRational__MPInteger_;
    procedure assign__MPRational_mpq_t;
    procedure assign__MPRational_string;
    procedure assign_mpz_t_MPInteger_;
    procedure assign_randstate_t_MPRandState_;
    procedure assign_string_MPFloat_;
    procedure assign_string_MPInteger_;
    procedure assign_string_MPRational_;
    procedure assign_valsint_MPFloat_;
    procedure assign_valsint_MPInteger_;
    procedure assign_valsint_MPRational_;
    procedure assign_valuint_MPFloat_;
    procedure assign_valuint_MPInteger_;
    procedure assign_valuint_MPRational_;
    procedure lt__MPFloat__MPFloat_boolean;
    procedure lt__MPInteger__MPInteger_boolean;
    procedure lt__MPRational__MPRational_boolean;
    procedure le__MPFloat__MPFloat_boolean;
    procedure le__MPInteger__MPInteger_boolean;
    procedure le__MPRational__MPRational_boolean;
    procedure gt__MPFloat__MPFloat_boolean;
    procedure gt__MPInteger__MPInteger_boolean;
    procedure gt__MPRational__MPRational_boolean;
    procedure ge__MPFloat__MPFloat_boolean;
    procedure ge__MPInteger__MPInteger_boolean;
    procedure ge__MPRational__MPRational_boolean;
  end;

