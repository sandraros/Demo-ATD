*&---------------------------------------------------------------------*
*& Report z_atd_demo
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT z_atd_demo.

CLASS lcl_cut DEFINITION DEFERRED.

CLASS ltc_atd_aunit DEFINITION
  INHERITING FROM cl_aunit_assert
  FINAL
  FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS:
      setup,
      raise_exception_good        FOR TESTING RAISING cx_static_check,
      raise_exception_good2       FOR TESTING RAISING cx_static_check,
      raise_exception_bad         FOR TESTING RAISING cx_static_check,
      exporting_good              FOR TESTING RAISING cx_static_check,
      exporting_bad               FOR TESTING RAISING cx_static_check,
      exporting_returning_good    FOR TESTING RAISING cx_static_check,
      returning_good              FOR TESTING RAISING cx_static_check,
      exporting_twice_good        FOR TESTING RAISING cx_static_check,
      returning_twice_good        FOR TESTING RAISING cx_static_check,
      times_bad                   FOR TESTING RAISING cx_static_check,
      times_zero_bad              FOR TESTING RAISING cx_static_check,
      times_good                  FOR TESTING RAISING cx_static_check,
      times_good2                 FOR TESTING RAISING cx_static_check,
      importing_required_bad      FOR TESTING RAISING cx_static_check,
      importing_required_good     FOR TESTING RAISING cx_static_check,
      importing_required_good2    FOR TESTING RAISING cx_static_check,
      importing_optional_bad      FOR TESTING RAISING cx_static_check,
      importing_optional_good     FOR TESTING RAISING cx_static_check,
      importing_optional_good2    FOR TESTING RAISING cx_static_check,
      importing_optional_good3    FOR TESTING RAISING cx_static_check,
      changing_good               FOR TESTING RAISING cx_static_check,
      ignore_parameter_bad        FOR TESTING RAISING cx_static_check,
      ignore_parameter_good       FOR TESTING RAISING cx_static_check,
      ignore_all_parameters_bad   FOR TESTING RAISING cx_static_check,
      ignore_all_parameters_good  FOR TESTING RAISING cx_static_check,
      two_methods_bad             FOR TESTING RAISING cx_static_check,
      two_methods_good            FOR TESTING RAISING cx_static_check,
      several_cases_good          FOR TESTING RAISING cx_static_check,
      decision_table_good         FOR TESTING RAISING cx_static_check,
      decision_table_bad          FOR TESTING RAISING cx_static_check,
      decision_table_bad2         FOR TESTING RAISING cx_static_check,
      decision_table_bad3         FOR TESTING RAISING cx_static_check,
      decision_table_bad4         FOR TESTING RAISING cx_static_check,
      order_of_calls_bad          FOR TESTING RAISING cx_static_check,
      order_of_calls_bad2         FOR TESTING RAISING cx_static_check,
      raise_event_good            FOR TESTING RAISING cx_static_check.

    METHODS on_action_done FOR EVENT action_done OF zif_atd_demo IMPORTING param_name.
    METHODS decision_table_init_bad.

    DATA: test_double TYPE REF TO zif_atd_demo,
          cut         TYPE REF TO lcl_cut,
          result      TYPE i.
    DATA event_param_value TYPE string.

ENDCLASS.

CLASS ltc_atd_max_test_doubles DEFINITION
  INHERITING FROM cl_aunit_assert
  FINAL
  FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS max_test_doubles FOR TESTING.
ENDCLASS.

CLASS lcl_cut DEFINITION
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zif_atd_demo.
    METHODS constructor
      IMPORTING
        atd_aunit TYPE REF TO zif_atd_demo.

  PRIVATE SECTION.
    DATA: mr_atd_aunit TYPE REF TO zif_atd_demo.
ENDCLASS.

CLASS lcx_cut DEFINITION INHERITING FROM cx_no_check.
ENDCLASS.

CLASS lcl_cut IMPLEMENTATION.

  METHOD constructor.
    mr_atd_aunit = atd_aunit.
  ENDMETHOD.

  METHOD zif_atd_demo~exporting.
    mr_atd_aunit->exporting( IMPORTING result = result ).
  ENDMETHOD.

  METHOD zif_atd_demo~demo_raise_exception.
    mr_atd_aunit->demo_raise_exception( ).
  ENDMETHOD.

  METHOD zif_atd_demo~demo_raise_exception2.
    result = mr_atd_aunit->demo_raise_exception2( whatever ).
  ENDMETHOD.

  METHOD zif_atd_demo~demo_returning.
    result = mr_atd_aunit->demo_returning( ).
  ENDMETHOD.

  METHOD zif_atd_demo~importing_optional.
    IF whatever IS NOT SUPPLIED.
      result = mr_atd_aunit->importing_optional( ).
    ELSE.
      result = mr_atd_aunit->importing_optional( whatever ).
    ENDIF.
  ENDMETHOD.

  METHOD zif_atd_demo~importing_required.
    result = mr_atd_aunit->importing_required( whatever ).
  ENDMETHOD.

  METHOD zif_atd_demo~changing.
    mr_atd_aunit->changing( CHANGING whatever = whatever ).
  ENDMETHOD.

  METHOD zif_atd_demo~demo_raise_event.
    mr_atd_aunit->demo_raise_event( ).
  ENDMETHOD.

  METHOD zif_atd_demo~several_cases.
    result = mr_atd_aunit->several_cases( whatever = whatever ).
  ENDMETHOD.

  METHOD zif_atd_demo~decision_table.
    result = mr_atd_aunit->decision_table( p1 = p1 p2 = p2 ).
  ENDMETHOD.

  METHOD zif_atd_demo~exporting_returning.
    returning = mr_atd_aunit->exporting_returning( IMPORTING exporting = exporting ).
  ENDMETHOD.

ENDCLASS.

CLASS ltc_atd_aunit IMPLEMENTATION.

  METHOD setup.
    " create test double object
    test_double ?= cl_abap_testdouble=>create( 'ZIF_ATD_DEMO' ).
    " injecting the test double into the object to be tested (Code Under Test)
    CREATE OBJECT cut EXPORTING atd_aunit = test_double.
  ENDMETHOD.

  METHOD raise_exception_good.
    " GOOD EXAMPLE
    " When the method DEMO_RAISE_EXCEPTION is called
    " Then an exception should be raised
    DATA: lx_exp TYPE REF TO lcx_cut,
          lx_act TYPE REF TO lcx_cut.

    CREATE OBJECT lx_exp.
    cl_abap_testdouble=>configure_call( test_double )->raise_exception( lx_exp ).
    test_double->demo_raise_exception( ).

    TRY.
        cut->zif_atd_demo~demo_raise_exception( ).
      CATCH lcx_cut INTO lx_act.
    ENDTRY.

    assert_equals( exp = lx_exp act = lx_act ).
  ENDMETHOD.

  METHOD raise_exception_good2.
    " GOOD EXAMPLE
    " When the method DEMO_RAISE_EXCEPTION2 is called with parameter 20
    " Then an exception should be raised
    "
    " When the method DEMO_RAISE_EXCEPTION2 is called with parameter 30
    " Then an exception should not be raised and the returned value should be 88
    DATA: lx_exp TYPE REF TO lcx_cut,
          lx_act TYPE REF TO lcx_cut.

    CREATE OBJECT lx_exp.
    cl_abap_testdouble=>configure_call( test_double )->raise_exception( lx_exp ).
    test_double->demo_raise_exception2( 20 ).

    cl_abap_testdouble=>configure_call( test_double )->returning( 88 ).
    test_double->demo_raise_exception2( 30 ).

    TRY.
        cut->zif_atd_demo~demo_raise_exception2( 20 ).
      CATCH lcx_cut INTO lx_act.
    ENDTRY.
    assert_equals( exp = lx_exp act = lx_act ).

    result = cut->zif_atd_demo~demo_raise_exception2( 30 ).
    assert_equals( exp = 88 act = result ).
  ENDMETHOD.

  METHOD raise_exception_bad.
    " BAD EXAMPLE
    " When the call configuration uses both RETURNING and RAISE_EXCEPTION
    " Then an exception '[ ABAP Testdouble Framework ] Illegal combination
    "       of RETURNING with RAISE_EXCEPTION' should be raised
    "
    " When the call configuration uses both SET_PARAMETER and RAISE_EXCEPTION
    " Then an exception 'Illegal config call. Previous
    "       configuration not complete' should be raised
    DATA: lx_exp TYPE REF TO lcx_cut,
          lx2    TYPE REF TO cx_atd_exception.

    CREATE OBJECT lx_exp.
    TRY.
        cl_abap_testdouble=>configure_call( test_double )->raise_exception( lx_exp )->returning( 90 ).
        fail( ).
      CATCH cx_atd_exception INTO lx2.
        assert_equals( exp = cx_atd_exception=>illegal_combi_exception
                       act = lx2->if_t100_message~t100key ).
    ENDTRY.

    TRY.
        cl_abap_testdouble=>configure_call( test_double )->raise_exception( lx_exp )->set_parameter( name = 'OUTPUT' value = 90 ).
        fail( ).
      CATCH cx_atd_exception INTO lx2.
        assert_equals( exp = cx_atd_exception=>illegal_config_call
                       act = lx2->if_t100_message~t100key ).
    ENDTRY.
  ENDMETHOD.

  METHOD exporting_bad.
    " BAD EXAMPLE
    " When the call configuration contains IMPORTING
    " Then an ABAP Test Double exception should be raised
    " Solution: retrieve the results via SET_PARAMETER in the CONFIGURE_CALL
    DATA lx2 TYPE REF TO cx_atd_exception.

    cl_abap_testdouble=>configure_call( test_double )->set_parameter( name = 'RESULT' value = 20 ).
    TRY.
        test_double->exporting( IMPORTING result = result ).
      CATCH cx_atd_exception INTO lx2.
    ENDTRY.
    assert_bound( lx2 ).
    assert_equals( exp = cx_atd_exception=>export_not_allowed
                   act = lx2->if_t100_message~t100key ).
  ENDMETHOD.

  METHOD exporting_good.
    " GOOD EXAMPLE
    " When the method EXPORTING is called
    " Then it should return the exporting parameter RESULT with value 20
    cl_abap_testdouble=>configure_call( test_double )->set_parameter( name = 'RESULT' value = 20 ).
    test_double->exporting( ).

    cut->zif_atd_demo~exporting( IMPORTING result = result ).
    assert_equals( exp = 20 act = result ).
  ENDMETHOD.

  METHOD returning_good.
    " GOOD EXAMPLE
    " When the method DEMO_RETURNING is called
    " Then it should return the returning parameter RESULT with value 97
    cl_abap_testdouble=>configure_call( test_double )->returning( 97 ).
    test_double->demo_returning( ).

    result = cut->zif_atd_demo~demo_returning( ).

    assert_equals( exp = 97 act = result ).
  ENDMETHOD.

  METHOD exporting_twice_good.
    " GOOD EXAMPLE
    " When the method EXPORTING is called multiple times
    " Then it should always return the exporting parameter RESULT with value 20
    cl_abap_testdouble=>configure_call( test_double )->set_parameter( name = 'RESULT' value = 20 ).
    test_double->exporting( ).

    cut->zif_atd_demo~exporting( IMPORTING result = result ).
    cut->zif_atd_demo~exporting( IMPORTING result = result ).

    assert_equals( exp = 20 act = result ).
  ENDMETHOD.

  METHOD returning_twice_good.
    " GOOD EXAMPLE
    " When the method DEMO_RETURNING is called multiple times
    " Then it should return the returning parameter RESULT with value 97
    cl_abap_testdouble=>configure_call( test_double )->returning( 97 ).
    test_double->demo_returning( ).

    result = cut->zif_atd_demo~demo_returning( ).
    result = cut->zif_atd_demo~demo_returning( ).

    assert_equals( exp = 97 act = result ).
  ENDMETHOD.

  METHOD times_bad.
    " BAD EXAMPLE
    " When the method DEMO_RETURNING is called the first time
    " Then it should return the returning parameter RESULT with value 97
    "
    " When the method DEMO_RETURNING is called the second time
    " Then it should return the returning parameter RESULT with value 0
    " But it returns 97 !
    " Solution: you should define another call configuration, because
    " the last one is always returned.
    cl_abap_testdouble=>configure_call( test_double
          )->returning( 97
          )->times( 1 ). " useless, times( 1 ) is the default if not specified
    test_double->demo_returning( ).

    result = cut->zif_atd_demo~demo_returning( ).
    result = cut->zif_atd_demo~demo_returning( ).

    assert_equals( exp = 97 act = result ).
  ENDMETHOD.

  METHOD times_good.
    " GOOD EXAMPLE
    " When the method DEMO_RETURNING is called the first time
    " Then it should return the returning parameter RESULT with value 97
    "
    " When the method DEMO_RETURNING is called the next times
    " Then it should return the returning parameter RESULT with value 0
    cl_abap_testdouble=>configure_call( test_double
          )->returning( 97 ).
    test_double->demo_returning( ).
    cl_abap_testdouble=>configure_call( test_double
          )->returning( 0 ).
    test_double->demo_returning( ).

    result = cut->zif_atd_demo~demo_returning( ).
    assert_equals( exp = 97 act = result ).

    result = cut->zif_atd_demo~demo_returning( ).
    assert_equals( exp = 0 act = result ).

    " from now on (max times consumed), the last configuration is used
    result = cut->zif_atd_demo~demo_returning( ).
    assert_equals( exp = 0 act = result ).
  ENDMETHOD.

  METHOD times_good2.
    " GOOD EXAMPLE
    " When the method DEMO_RETURNING is called the two first times
    " Then it should return the returning parameter RESULT with value 97
    "
    " When the method DEMO_RETURNING is called the next times
    " Then it should return the returning parameter RESULT with value 0
    cl_abap_testdouble=>configure_call( test_double
          )->returning( 97
          )->times( 2 ).
    test_double->demo_returning( ).
    cl_abap_testdouble=>configure_call( test_double
          )->returning( 0 ).
    test_double->demo_returning( ).

    result = cut->zif_atd_demo~demo_returning( ).
    assert_equals( exp = 97 act = result ).

    result = cut->zif_atd_demo~demo_returning( ).
    assert_equals( exp = 97 act = result ).

    result = cut->zif_atd_demo~demo_returning( ).
    assert_equals( exp = 0 act = result ).

    " from now on (max times consumed), the last configuration is used
    result = cut->zif_atd_demo~demo_returning( ).
    assert_equals( exp = 0 act = result ).
  ENDMETHOD.

  METHOD times_zero_bad.
    " TIMES( 0 ) doesn't mean unlimited number of times
    DATA lx2 TYPE REF TO cx_atd_exception.
    TRY.
        cl_abap_testdouble=>configure_call( test_double
              )->returning( 97
              )->times( 0 ). " <--- INVALID
      CATCH cx_atd_exception INTO lx2.
    ENDTRY.
    assert_bound( lx2 ).
    assert_equals( exp = cx_atd_exception=>times_zero
                   act = lx2->if_t100_message~t100key ).
  ENDMETHOD.

  METHOD exporting_returning_good.
    " GOOD EXAMPLE
    " When the method EXPORTING_RETURNING is called
    " Then it should return the returning parameter with value 97
    "  and the exporting parameter with value 135
    DATA: exporting TYPE i.

    cl_abap_testdouble=>configure_call( test_double
          )->returning( 97
          )->set_parameter( name = 'EXPORTING' value = 135 ).
    test_double->exporting_returning( ).

    result = cut->zif_atd_demo~exporting_returning( IMPORTING exporting = exporting ).

    assert_equals( exp = 97 act = result ).
    assert_equals( exp = 135 act = exporting ).
  ENDMETHOD.

  METHOD importing_required_bad.
    " BAD EXAMPLE
    " When the method IMPORTING_REQUIRED is called with parameter 5555
    " Then it should return 97
    " But it returns 0 !
    " Solution: replace 5554 with 5555 in the call configuration
    cl_abap_testdouble=>configure_call( test_double )->returning( 97 ).
    test_double->importing_required( 5554 ).

    result = cut->zif_atd_demo~importing_required( 5555 ).

    assert_equals( exp = 0 act = result ).
  ENDMETHOD.

  METHOD importing_required_good.
    " GOOD EXAMPLE
    " When the method IMPORTING_REQUIRED is called with parameter 5555
    " Then it should return 97
    cl_abap_testdouble=>configure_call( test_double )->returning( 97 ).
    test_double->importing_required( 5555 ).

    result = cut->zif_atd_demo~importing_required( 5555 ).

    assert_equals( exp = 97 act = result ).
  ENDMETHOD.

  METHOD importing_required_good2.
    " GOOD EXAMPLE
    " When the method IMPORTING_REQUIRED is called with any parameter value
    " Then it should return 97
    " (note: this test method is the duplicate of IGNORE_PARAMETER_GOOD)
    cl_abap_testdouble=>configure_call( test_double
          )->ignore_parameter( 'WHATEVER' )->returning( 97 ).
    test_double->importing_required( whatever = 5554 ). " any value, it's ignored

    result = cut->zif_atd_demo~importing_required( 5555 ).

    assert_equals( exp = 97 act = result ).
  ENDMETHOD.

  METHOD importing_optional_bad.
    " BAD EXAMPLE
    " When the method IMPORTING_OPTIONAL is called with the optional parameter equal to any value,
    " Then it should return 97
    " But it returns 0 !
    " That doesn't work because you should use the method IGNORE_PARAMETER to indicate "any value".
    cl_abap_testdouble=>configure_call( test_double )->returning( 97 ).
    test_double->importing_optional( ).

    result = cut->zif_atd_demo~importing_optional( 5555 ).

    assert_equals( exp = 0 act = result ).
  ENDMETHOD.

  METHOD importing_optional_good.
    " GOOD EXAMPLE
    " When the method IMPORTING_OPTIONAL is called with the optional parameter equal to any value,
    " Then it should return 97
    cl_abap_testdouble=>configure_call( test_double
          )->ignore_parameter( 'WHATEVER' )->returning( 97 ).
    test_double->importing_optional( ).

    result = cut->zif_atd_demo~importing_optional( whatever = 5555 ).

    assert_equals( exp = 97 act = result ).
  ENDMETHOD.

  METHOD importing_optional_good2.
    " GOOD EXAMPLE
    " When the method IMPORTING_OPTIONAL is called without the optional parameter
    " Then it should return 97
    cl_abap_testdouble=>configure_call( test_double )->returning( 97 ).
    test_double->importing_optional( ).

    result = cut->zif_atd_demo~importing_optional( ).

    assert_equals( exp = 97 act = result ).
  ENDMETHOD.

  METHOD importing_optional_good3.
    " GOOD EXAMPLE
    " When the method IMPORTING_OPTIONAL is called with the optional parameter 5555
    " Then it should return 97
    cl_abap_testdouble=>configure_call( test_double )->returning( 97 ).
    test_double->importing_optional( 5555 ).

    result = cut->zif_atd_demo~importing_optional( 5555 ).

    assert_equals( exp = 97 act = result ).
  ENDMETHOD.

  METHOD changing_good.
    " GOOD EXAMPLE
    " When the method CHANGING is called with the changing parameter equal to 0
    " Then it should be changed to 40
    "
    " When the method CHANGING is called with the changing parameter equal to 50
    " Then it should be changed to 110
    DATA: whatever TYPE i.

    cl_abap_testdouble=>configure_call( test_double
          )->set_parameter( name = 'WHATEVER' value = 40 ).
    whatever = 0.
    test_double->changing( CHANGING whatever = whatever ).

    cl_abap_testdouble=>configure_call( test_double
          )->set_parameter( name = 'WHATEVER' value = 110 ).
    whatever = 50.
    test_double->changing( CHANGING whatever = whatever ).

    whatever = 0.
    cut->zif_atd_demo~changing( CHANGING whatever = whatever ).
    whatever = whatever + 10.
    cut->zif_atd_demo~changing( CHANGING whatever = whatever ).

    assert_equals( exp = 110 act = whatever ).
  ENDMETHOD.

  METHOD ignore_parameter_bad.
    " BAD EXAMPLE
    " When the method IMPORTING_REQUIRED is called with any parameter value
    " Then it should return 97
    " But it returns 0 !
    " Solution: you missed the second part of the call configuration!
    cl_abap_testdouble=>configure_call( test_double
          )->ignore_parameter( 'WHATEVER' )->returning( 97 ).

    result = cut->zif_atd_demo~importing_required( 5555 ).

    assert_equals( exp = 0 act = result ).
  ENDMETHOD.

  METHOD ignore_parameter_good.
    " GOOD EXAMPLE
    " When the method IMPORTING_REQUIRED is called with any parameter value
    " Then it should return 97
    cl_abap_testdouble=>configure_call( test_double
          )->ignore_parameter( 'WHATEVER' )->returning( 97 ).
    test_double->importing_required( 1234 ). " <=== important call & dummy value !

    result = cut->zif_atd_demo~importing_required( 5555 ).

    assert_equals( exp = 97 act = result ).
  ENDMETHOD.

  METHOD ignore_all_parameters_bad.
    " BAD EXAMPLE
    " When the method IMPORTING_REQUIRED is called with any parameter value
    " Then it should return 97
    " But it returns 0 !
    " Solution: you missed the second part of the call configuration!
    cl_abap_testdouble=>configure_call( test_double
          )->ignore_all_parameters( )->returning( 97 ).

    result = cut->zif_atd_demo~importing_required( 5555 ).

    assert_equals( exp = 0 act = result ).
  ENDMETHOD.

  METHOD ignore_all_parameters_good.
    " GOOD EXAMPLE
    " When the method IMPORTING_REQUIRED is called with any parameter value
    " Then it should return 97
    cl_abap_testdouble=>configure_call( test_double
          )->ignore_all_parameters( )->returning( 97 ).
    test_double->importing_required( 5554 ). " (dummy value)

    result = cut->zif_atd_demo~importing_required( 5555 ).

    assert_equals( exp = 97 act = result ).
  ENDMETHOD.

  METHOD two_methods_bad.
    " BAD EXAMPLE
    " When the method IMPORTING_REQUIRED is called with any parameter value
    " Then it should return 97
    "
    " When the method DEMO_RETURNING is called
    " Then it should return 99
    " But it returns 0 !
    " Solution: you missed the first part of the call configuration! (to set 99)
    cl_abap_testdouble=>configure_call( test_double
          )->ignore_all_parameters( )->returning( 97 ).
    test_double->importing_required( 17 ). " (dummy value)

    test_double->demo_returning( ).

    result = cut->zif_atd_demo~importing_required( 5555 ).
    assert_equals( exp = 97 act = result ).

    result = test_double->demo_returning( ).
    assert_equals( exp = 0 act = result ).
  ENDMETHOD.

  METHOD two_methods_good.
    " GOOD EXAMPLE
    " When the method IMPORTING_REQUIRED is called with any parameter value
    " Then it should return 97
    "
    " When the method DEMO_RETURNING is called
    " Then it should return 99
    cl_abap_testdouble=>configure_call( test_double
          )->ignore_all_parameters( )->returning( 97 ).
    test_double->importing_required( 17 ). " (dummy value)

    cl_abap_testdouble=>configure_call( test_double )->returning( 99 ).
    test_double->demo_returning( ).

    result = cut->zif_atd_demo~importing_required( 5555 ).
    assert_equals( exp = 97 act = result ).

    result = test_double->demo_returning( ).
    assert_equals( exp = 99 act = result ).
  ENDMETHOD.

  METHOD several_cases_good.
    " GOOD EXAMPLE
    " When the method SEVERAL_CASES is called with value 27
    " Then it should return 40
    "
    " When the method SEVERAL_CASES is called with value 29
    " Then it should return 70
    cl_abap_testdouble=>configure_call( test_double )->returning( 40 ).
    test_double->several_cases( 27 ).

    cl_abap_testdouble=>configure_call( test_double )->returning( 70 ).
    test_double->several_cases( 29 ).

    result = cut->zif_atd_demo~several_cases( 27 ).
    assert_equals( exp = 40 act = result ).

    result = cut->zif_atd_demo~several_cases( 29 ).
    assert_equals( exp = 70 act = result ).

    result = cut->zif_atd_demo~several_cases( 27 ).
    assert_equals( exp = 40 act = result ).
  ENDMETHOD.

  METHOD decision_table_good.
    " case which returns 10
    cl_abap_testdouble=>configure_call( test_double
          )->returning( 10 )->times( 999999999 ).
    test_double->decision_table( p1 = 5 p2 = 5 ).
    " case which returns 20
    cl_abap_testdouble=>configure_call( test_double
          )->ignore_parameter( 'P1' )->returning( 20 )->times( 999999999 ).
    test_double->decision_table( p1 = 0 p2 = 5 ).
    " case which returns 30
    cl_abap_testdouble=>configure_call( test_double
          )->ignore_parameter( 'P2' )->returning( 30 )->times( 999999999 ).
    test_double->decision_table( p1 = 5 p2 = 0 ).
    " case which returns 40
    cl_abap_testdouble=>configure_call( test_double
          )->ignore_parameter( 'P1' )->ignore_parameter( 'P2'
          )->returning( 40 )->times( 999999999 ).
    test_double->decision_table( p1 = 0 p2 = 0 ).

    " When there's only one matching case
    " Then the result is trivial
    result = cut->zif_atd_demo~decision_table( p1 = 66 p2 = 66 ).
    assert_equals( exp = 40 act = result ).
    result = cut->zif_atd_demo~decision_table( p1 = 66 p2 = 66 ).
    assert_equals( exp = 40 act = result ).
    " Scenario Outline
    " When there are several matching cases
    " Then all of them are returned in turn
    " Examples :
    " 1) The possible cases are 30 and 40:
    result = cut->zif_atd_demo~decision_table( p1 = 5 p2 = 66 ).
    assert_equals( exp = 30 act = result ).
    result = cut->zif_atd_demo~decision_table( p1 = 5 p2 = 66 ).
    assert_equals( exp = 30 act = result ).
    " 2) The possible cases are 20 and 40:
    result = cut->zif_atd_demo~decision_table( p1 = 66 p2 = 5 ).
    assert_equals( exp = 20 act = result ).
    result = cut->zif_atd_demo~decision_table( p1 = 66 p2 = 5 ).
    assert_equals( exp = 20 act = result ).
    " 3) The possible cases are 10, 20, 30 and 40:
    result = cut->zif_atd_demo~decision_table( p1 = 5 p2 = 5 ).
    assert_equals( exp = 10 act = result ).
    result = cut->zif_atd_demo~decision_table( p1 = 5 p2 = 5 ).
    assert_equals( exp = 10 act = result ).
  ENDMETHOD.

  METHOD decision_table_bad.
    decision_table_init_bad( ).

    " When there's only one matching case
    " Then the result is trivial
    result = cut->zif_atd_demo~decision_table( p1 = 66 p2 = 66 ).
    assert_equals( exp = 40 act = result ).

    " Scenario Outline
    " When there are several matching cases
    " Then all of them are returned in turn
    " Examples :
    " 1) The possible cases are 30 and 40:
    result = cut->zif_atd_demo~decision_table( p1 = 5 p2 = 66 ).
    assert_equals( exp = 30 act = result ).
    result = cut->zif_atd_demo~decision_table( p1 = 5 p2 = 66 ).
    assert_equals( exp = 40 act = result ).
    result = cut->zif_atd_demo~decision_table( p1 = 5 p2 = 66 ).
    assert_equals( exp = 40 act = result ).
  ENDMETHOD.

  METHOD decision_table_bad2.
    decision_table_init_bad( ).

    " Scenario Outline
    " When there are several matching cases
    " Then all of them are returned in turn
    " Examples :
    " 2) The possible cases are 20 and 40:
    result = cut->zif_atd_demo~decision_table( p1 = 66 p2 = 5 ).
    assert_equals( exp = 20 act = result ).
    result = cut->zif_atd_demo~decision_table( p1 = 66 p2 = 5 ).
    assert_equals( exp = 40 act = result ).
    result = cut->zif_atd_demo~decision_table( p1 = 66 p2 = 5 ).
    assert_equals( exp = 40 act = result ).
  ENDMETHOD.

  METHOD decision_table_bad3.
    decision_table_init_bad( ).

    " Scenario Outline
    " When there are several matching cases
    " Then all of them are returned in turn
    " Examples :
    " 3) The possible cases are 10, 20, 30 and 40:
    result = cut->zif_atd_demo~decision_table( p1 = 5 p2 = 5 ).
    assert_equals( exp = 10 act = result ).
    result = cut->zif_atd_demo~decision_table( p1 = 5 p2 = 5 ).
    assert_equals( exp = 20 act = result ).
    result = cut->zif_atd_demo~decision_table( p1 = 5 p2 = 5 ).
    assert_equals( exp = 30 act = result ).
    result = cut->zif_atd_demo~decision_table( p1 = 5 p2 = 5 ).
    assert_equals( exp = 40 act = result ).
    result = cut->zif_atd_demo~decision_table( p1 = 5 p2 = 5 ).
    assert_equals( exp = 40 act = result ).
  ENDMETHOD.

  METHOD decision_table_bad4.
    decision_table_init_bad( ).

    " Scenario Outline
    " When there are several matching cases
    " Then all of them are returned in turn
    " Examples :
    " 1) The possible cases are 30 and 40:
    result = cut->zif_atd_demo~decision_table( p1 = 5 p2 = 66 ).
    assert_equals( exp = 30 act = result ).
    result = cut->zif_atd_demo~decision_table( p1 = 5 p2 = 66 ).
    assert_equals( exp = 40 act = result ).
    result = cut->zif_atd_demo~decision_table( p1 = 5 p2 = 66 ).
    assert_equals( exp = 40 act = result ).
    " 2) The possible cases are 20 and 40:
    result = cut->zif_atd_demo~decision_table( p1 = 66 p2 = 5 ).
    assert_equals( exp = 20 act = result ).
    result = cut->zif_atd_demo~decision_table( p1 = 66 p2 = 5 ).
    assert_equals( exp = 40 act = result ).
    result = cut->zif_atd_demo~decision_table( p1 = 66 p2 = 5 ).
    assert_equals( exp = 40 act = result ).
    " 3) The possible cases are 10, 20, 30 and 40:
    result = cut->zif_atd_demo~decision_table( p1 = 5 p2 = 5 ).
    assert_equals( exp = 10 act = result ).
    result = cut->zif_atd_demo~decision_table( p1 = 5 p2 = 5 ).
    assert_equals( exp = 40 act = result ).
    result = cut->zif_atd_demo~decision_table( p1 = 5 p2 = 5 ).
    assert_equals( exp = 40 act = result ).
  ENDMETHOD.

  METHOD decision_table_init_bad.
    " case which returns 10
    cl_abap_testdouble=>configure_call( test_double
          )->returning( 10 ).
    test_double->decision_table( p1 = 5 p2 = 5 ).

    " case which returns 20
    cl_abap_testdouble=>configure_call( test_double
          )->ignore_parameter( 'P1' )->returning( 20 ).
    test_double->decision_table( p1 = 0 p2 = 5 ).

    " case which returns 30
    cl_abap_testdouble=>configure_call( test_double
          )->ignore_parameter( 'P2' )->returning( 30 ).
    test_double->decision_table( p1 = 5 p2 = 0 ).

    " case which returns 40
    cl_abap_testdouble=>configure_call( test_double
          )->ignore_parameter( 'P1' )->ignore_parameter( 'P2' )->returning( 40 ).
    test_double->decision_table( p1 = 0 p2 = 0 ).
  ENDMETHOD.

  METHOD order_of_calls_bad.
    " Wrong call order: those 2 next lines should be switched
    test_double->demo_returning( ).
    cl_abap_testdouble=>configure_call( test_double )->returning( 97 ).

    result = cut->zif_atd_demo~demo_returning( ).

    " The result is 0 instead of 97 because of the wrong call order
    assert_equals( exp = 0 act = result ).
  ENDMETHOD.

  METHOD order_of_calls_bad2.
    DATA lx2 TYPE REF TO cx_atd_exception.
    DATA(callconfig) = cl_abap_testdouble=>configure_call( test_double ).
    test_double->demo_returning( ).
    TRY.
        callconfig->returning( 97 ).

        result = cut->zif_atd_demo~demo_returning( ).

      CATCH cx_atd_exception INTO lx2.
    ENDTRY.
    assert_bound( lx2 ).
    assert_equals( exp = cx_atd_exception=>invalid_call
                   act = lx2->if_t100_message~t100key ).
  ENDMETHOD.

  METHOD raise_event_good.
    SET HANDLER on_action_done FOR test_double.

    cl_abap_testdouble=>configure_call( test_double
          )->raise_event( name = 'ACTION_DONE'
              parameters = VALUE #( ( name = 'PARAM_NAME' value = REF i( 20 ) ) )
          )->raise_event( name = 'ACTION_DONE'
              parameters = VALUE #( ( name = 'PARAM_NAME' value = REF i( 50 ) ) ) ).
    " the 2 events should be triggered when the method RAISE_EVENT is called
    test_double->demo_raise_event( ).

    cut->zif_atd_demo~demo_raise_event( ).

    assert_equals( exp = 70 act = event_param_value ).
  ENDMETHOD.

  METHOD on_action_done.
    " This method is used for testing the method RAISE_EVENT.
    event_param_value = event_param_value + param_name.
  ENDMETHOD.

ENDCLASS.

CLASS ltc_atd_max_test_doubles IMPLEMENTATION.

  METHOD max_test_doubles.
    DATA: test_double TYPE REF TO object,
          lx          TYPE REF TO cx_sy_generate_subpool_full.

    SELECT
          seoclass~clsname AS name
          FROM seoclass
            INNER JOIN repoload
              ON concat( rpad( seoclass~clsname, 30, '=' ), 'IP' ) = repoload~progname
          WHERE seoclass~clstype = '1'
            AND seoclass~clsname LIKE 'IF_ADT_%'
            AND ( repoload~udat > repoload~sdat OR
                ( repoload~udat = repoload~sdat AND repoload~utime >= repoload~stime ) )
          INTO TABLE @DATA(interfaces)
          UP TO 37 ROWS.

    ASSERT lines( interfaces ) = 37.

    LOOP AT interfaces FROM 1 TO 36 ASSIGNING FIELD-SYMBOL(<interface>).
      test_double ?= cl_abap_testdouble=>create( <interface>-name ).
    ENDLOOP.

    TRY.
        " 37th GENERATE SUBROUTINE POOL
        test_double ?= cl_abap_testdouble=>create( interfaces[ 37 ]-name ).
      CATCH cx_sy_generate_subpool_full INTO lx.
    ENDTRY.
    assert_bound( lx ).
    assert_equals( exp = 'GENERATE_SUBPOOL_DIR_FULL' act = lx->kernel_errid ).

  ENDMETHOD.

ENDCLASS.
