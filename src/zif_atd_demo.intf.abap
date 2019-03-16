INTERFACE zif_atd_demo
  PUBLIC .

  EVENTS action_done
    EXPORTING
      VALUE(param_name) TYPE i .

  METHODS demo_raise_event .

  METHODS several_cases
    IMPORTING
      !whatever     TYPE i
    RETURNING
      VALUE(result) TYPE i .

  METHODS decision_table
    IMPORTING
      !p1           TYPE i
      !p2           TYPE i
    RETURNING
      VALUE(result) TYPE i .

  METHODS demo_raise_exception.

  METHODS demo_raise_exception2
    IMPORTING
      !whatever TYPE i OPTIONAL
    RETURNING
      VALUE(result) TYPE i .

  METHODS exporting
    EXPORTING
      !result TYPE i .

  METHODS importing_required
    IMPORTING
      !whatever     TYPE i
    RETURNING
      VALUE(result) TYPE i .

  METHODS importing_optional
    IMPORTING
      !whatever     TYPE i DEFAULT 33
    RETURNING
      VALUE(result) TYPE i .

  METHODS demo_returning
    RETURNING
      VALUE(result) TYPE i .

  METHODS exporting_returning
    EXPORTING
      !exporting       TYPE i
    RETURNING
      VALUE(returning) TYPE i .

  METHODS changing
    CHANGING
      !whatever TYPE i .

ENDINTERFACE.
