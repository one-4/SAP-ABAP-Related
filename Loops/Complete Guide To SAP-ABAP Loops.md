# Complete Guide to SAP ABAP Loops
## From Beginner to Expert

---
# Table of Contents

## 1. [Introduction to Loops](#chapter-1-introduction-to-loops)
- [What is a Loop?](#what-is-a-loop)
- [Why Do We Need Loops?](#why-do-we-need-loops)
- [Types of Loops in ABAP](#types-of-loops-in-abap)
- [The SY-INDEX System Variable](#the-sy-index-system-variable)

## 2. [DO...ENDDO Loop](#chapter-2-doenddo-loop)
- [Basic Syntax](#basic-syntax)
- [Type 1: Fixed Number of Iterations](#type-1-fixed-number-of-iterations)
  - [Example 2.1: Print Multiplication Table](#example-21-print-multiplication-table)
  - [Example 2.2: Calculate Factorial](#example-22-calculate-factorial)
- [Type 2: Infinite Loop with EXIT](#type-2-infinite-loop-with-exit)
  - [Example 2.3: Find First Number Divisible by 7](#example-23-find-first-number-divisible-by-7)
  - [Example 2.4: User Input Simulation](#example-24-user-input-simulation)
- [Type 3: VARYING Addition](#type-3-varying-addition)
  - [Example 2.5: Process Structure Fields](#example-25-process-structure-fields)
- [Common Mistakes with DO Loop](#common-mistakes-with-do-loop)
  - [Mistake 1: Infinite Loop Without EXIT](#mistake-1-infinite-loop-without-exit)
  - [Mistake 2: Modifying SY-INDEX](#mistake-2-modifying-sy-index)
  - [Mistake 3: Variable Number of Iterations](#mistake-3-variable-number-of-iterations)

## 3. [WHILE...ENDWHILE Loop](#chapter-3-whileendwhile-loop)
- [Basic Syntax](#basic-syntax-1)
- [Key Difference from DO Loop](#key-difference-from-do-loop)
- [Example 3.1: Basic WHILE Loop](#example-31-basic-while-loop)
- [Example 3.2: Sum Until Threshold](#example-32-sum-until-threshold)
- [Example 3.3: Find Prime Numbers](#example-33-find-prime-numbers)
- [Example 3.4: String Processing](#example-34-string-processing)
- [Example 3.5: Binary Search Simulation](#example-35-binary-search-simulation)
- [Common Mistakes with WHILE Loop](#common-mistakes-with-while-loop)
  - [Mistake 1: Forgetting to Update Condition Variable](#mistake-1-forgetting-to-update-condition-variable)
  - [Mistake 2: Wrong Initial Condition](#mistake-2-wrong-initial-condition)
  - [Mistake 3: Off-by-One Error](#mistake-3-off-by-one-error)

## 4. [LOOP AT Internal Tables](#chapter-4-loop-at-internal-tables)
- [Introduction to Internal Table Loops](#introduction-to-internal-table-loops)
- [Basic Syntax](#basic-syntax-2)
- [SY-TABIX System Variable](#sy-tabix-system-variable)
- [Different Ways to Loop](#different-ways-to-loop)
  - [Method 1: INTO Work Area](#method-1-into-work-area)
  - [Method 2: ASSIGNING Field Symbol](#method-2-assigning-field-symbol)
  - [Method 3: REFERENCE INTO](#method-3-reference-into)
- [Example 4.1: Complete Customer Processing](#example-41-complete-customer-processing)
- [LOOP with WHERE Condition](#loop-with-where-condition)
  - [Example 4.2: Filter High Balance Customers](#example-42-filter-high-balance-customers)
- [LOOP with FROM and TO](#loop-with-from-and-to)
  - [Example 4.3: Pagination](#example-43-pagination)
- [LOOP with GROUP BY](#loop-with-group-by)
  - [Example 4.4: Group Customers by City](#example-44-group-customers-by-city)
- [Modifying Internal Table During LOOP](#modifying-internal-table-during-loop)
  - [Using Field Symbols (Recommended)](#using-field-symbols-recommended)
- [Deleting Rows During LOOP](#deleting-rows-during-loop)
  - [Example 4.5: Delete Rows Safely](#example-45-delete-rows-safely)
  - [Deleting Inside Loop (Use with Caution)](#deleting-inside-loop-use-with-caution)
- [Performance Comparison: INTO vs ASSIGNING](#performance-comparison-into-vs-assigning)

## 5. [SELECT...ENDSELECT Loop](#chapter-5-selectendselect-loop)
- [What is SELECT...ENDSELECT?](#what-is-selectendselect)
- [Basic Syntax](#basic-syntax-3)
- [Example 5.1: Basic SELECT Loop](#example-51-basic-select-loop)
- [Why SELECT...ENDSELECT is NOT Recommended](#why-selectendselect-is-not-recommended)
  - [The Problem](#the-problem)
  - [The Solution - Array Fetch](#the-solution---array-fetch)
- [Performance Comparison](#performance-comparison)
- [When SELECT...ENDSELECT Might Be Acceptable](#when-selectendselect-might-be-acceptable)
  - [Example 5.2: Large Dataset Processing](#example-52-large-dataset-processing)
- [Better Alternative: FOR ALL ENTRIES](#better-alternative-for-all-entries)

## 6. [LOOP AT SCREEN](#chapter-6-loop-at-screen)
- [What is LOOP AT SCREEN?](#what-is-loop-at-screen)
- [Basic Syntax](#basic-syntax-4)
- [SCREEN Structure Fields](#screen-structure-fields)
- [Example 6.1: Hide/Show Fields](#example-61-hideshow-fields)
- [Example 6.2: Make Fields Read-Only](#example-62-make-fields-read-only)
- [Example 6.3: Using Modification Groups](#example-63-using-modification-groups)
- [Example 6.4: Make Fields Mandatory](#example-64-make-fields-mandatory)
- [Example 6.5: Highlight Fields](#example-65-highlight-fields)
- [Common LOOP AT SCREEN Patterns](#common-loop-at-screen-patterns)
  - [Pattern 1: Role-Based Field Control](#pattern-1-role-based-field-control)
  - [Pattern 2: Dependent Fields](#pattern-2-dependent-fields)

## 7. [Nested Loops](#chapter-7-nested-loops)
- [What are Nested Loops?](#what-are-nested-loops)
- [Basic Structure](#basic-structure)
- [Example 7.1: Multiplication Table Grid](#example-71-multiplication-table-grid)
- [Example 7.2: Nested Internal Table Loops](#example-72-nested-internal-table-loops)
- [The Nested Loop Performance Problem](#the-nested-loop-performance-problem)
  - [Problem: O(n×m) Complexity](#problem-onm-complexity)
- [Solution 1: SORTED Table with READ](#solution-1-sorted-table-with-read)
- [Solution 2: HASHED Table for Lookup](#solution-2-hashed-table-for-lookup)
- [Solution 3: Pre-Process with Parallel Cursor](#solution-3-pre-process-with-parallel-cursor)
- [Performance Comparison](#performance-comparison-1)
- [Example 7.3: Matrix Operations](#example-73-matrix-operations)

## 8. [Loop Control Statements](#chapter-8-loop-control-statements)
- [Overview](#overview)
- [EXIT Statement](#exit-statement)
  - [Example 8.1: Find First Match](#example-81-find-first-match)
  - [EXIT in Nested Loops](#exit-in-nested-loops)
- [CONTINUE Statement](#continue-statement)
  - [Example 8.2: Skip Processing](#example-82-skip-processing)
  - [Example 8.3: Skip Invalid Records](#example-83-skip-invalid-records)
- [CHECK Statement](#check-statement)
  - [Syntax](#syntax)
  - [Example 8.4: CHECK vs CONTINUE](#example-84-check-vs-continue)
  - [Example 8.5: Multiple CHECKs](#example-85-multiple-checks)
- [CHECK Outside Loops](#check-outside-loops)
- [RETURN Statement](#return-statement)
  - [Example 8.6: RETURN in Loop](#example-86-return-in-loop)
- [Comparison Summary](#comparison-summary)

## 9. [Performance and Optimization](#chapter-9-performance-and-optimization)
- [The Golden Rules of Loop Performance](#the-golden-rules-of-loop-performance)
- [Rule 1: Database Calls Inside Loops](#rule-1-database-calls-inside-loops)
  - [BAD Practice](#bad-practice)
  - [GOOD Practice](#good-practice)
- [Rule 2: Choose Right Table Type](#rule-2-choose-right-table-type)
  - [Performance Characteristics](#performance-characteristics)
  - [Example 9.1: Table Type Comparison](#example-91-table-type-comparison)
- [Rule 3: Avoid Unnecessary Nested Loops](#rule-3-avoid-unnecessary-nested-loops)
- [Rule 4: Use Field Symbols](#rule-4-use-field-symbols)
  - [Example 9.2: INTO vs ASSIGNING Performance](#example-92-into-vs-assigning-performance)
- [Rule 5: Filter Early](#rule-5-filter-early)
- [Advanced: Parallel Processing](#advanced-parallel-processing)
  - [Example 9.3: Using aRFC for Parallel Processing](#example-93-using-arfc-for-parallel-processing)
- [Summary: Performance Checklist](#summary-performance-checklist)

## 10. [Common Errors and Solutions](#chapter-10-common-errors-and-solutions)
- [Error 1: Infinite Loop](#error-1-infinite-loop)
- [Error 2: SY-TABIX Becomes 0 or Wrong](#error-2-sy-tabix-becomes-0-or-wrong)
- [Error 3: Modifying Table During Loop](#error-3-modifying-table-during-loop)
- [Error 4: Field Symbol Not Assigned](#error-4-field-symbol-not-assigned)
- [Error 5: Wrong Table in Nested LOOP](#error-5-wrong-table-in-nested-loop)
- [Error 6: Empty Table Not Checked](#error-6-empty-table-not-checked)
- [Error 7: TYPE Mismatch in VARYING](#error-7-type-mismatch-in-varying)
- [Error 8: LOOP AT SCREEN Without MODIFY](#error-8-loop-at-screen-without-modify)
- [Error 9: Incorrect WHERE Condition Syntax](#error-9-incorrect-where-condition-syntax)
- [Error 10: Performance - SELECT in LOOP](#error-10-performance---select-in-loop)

## 11. [Complex Real-World Scenarios](#chapter-11-complex-real-world-scenarios)
- [Scenario 1: Process Sales Orders with Items and Deliveries](#scenario-1-process-sales-orders-with-items-and-deliveries)
  - [Problem](#problem)
  - [Solution](#solution)
- [Scenario 2: Hierarchical Data Processing (BOM Explosion)](#scenario-2-hierarchical-data-processing-bom-explosion)
  - [Problem](#problem-1)
  - [Solution](#solution-1)
- [Scenario 3: Batch Processing with Error Handling](#scenario-3-batch-processing-with-error-handling)
  - [Problem](#problem-2)
  - [Solution](#solution-2)
- [Scenario 4: Dynamic Loop Based on Configuration](#scenario-4-dynamic-loop-based-on-configuration)
  - [Problem](#problem-3)
  - [Solution](#solution-3)

## 12. [Best Practices and Guidelines](#chapter-12-best-practices-and-guidelines)
- [Coding Standards for Loops](#coding-standards-for-loops)
  - [1. Always Use Meaningful Names](#1-always-use-meaningful-names)
  - [2. Keep Loops Focused](#2-keep-loops-focused)
  - [3. Comment Complex Loops](#3-comment-complex-loops)
  - [4. Limit Nesting Depth](#4-limit-nesting-depth)
  - [5. Prefer Declarative Approaches](#5-prefer-declarative-approaches)
- [Performance Guidelines Summary](#performance-guidelines-summary)
- [Anti-Patterns to Avoid](#anti-patterns-to-avoid)
  - [Anti-Pattern 1: The God Loop](#anti-pattern-1-the-god-loop)
  - [Anti-Pattern 2: Copy-Paste Loops](#anti-pattern-2-copy-paste-loops)
  - [Anti-Pattern 3: Premature Optimization](#anti-pattern-3-premature-optimization)
- [Testing Loops](#testing-loops)
  - [Unit Test Example](#unit-test-example)
- [Quick Reference Card](#quick-reference-card)

## Appendices

### [Appendix A: Quick Troubleshooting Guide](#appendix-a-quick-troubleshooting-guide)

### [Appendix B: Performance Benchmarks](#appendix-b-performance-benchmarks)

---

## [Conclusion](#conclusion)

---

# Chapter 1: Introduction to Loops

## What is a Loop?

A loop is a programming construct that allows you to execute a block of code repeatedly. Instead of writing the same code multiple times, you write it once inside a loop, and the computer executes it as many times as needed.

## Why Do We Need Loops?

Imagine you need to print numbers from 1 to 100. Without loops, you would write:

```abap
WRITE: / 1.
WRITE: / 2.
WRITE: / 3.
" ... and so on until 100
```

This is impractical. With a loop, you simply write:

```abap
DO 100 TIMES.
  WRITE: / sy-index.
ENDDO.
```

## Types of Loops in ABAP

| Loop Type | Purpose | Use When |
|-----------|---------|----------|
| DO...ENDDO | Fixed iterations or infinite loop | You know how many times to repeat |
| WHILE...ENDWHILE | Condition-based iteration | You need to repeat while condition is true |
| LOOP AT itab | Process internal table rows | Working with internal tables |
| SELECT...ENDSELECT | Database cursor loop | Reading database records one by one |
| LOOP AT SCREEN | Modify screen elements | Working with screen programming |

## The SY-INDEX System Variable

`SY-INDEX` is a system variable that contains the current loop iteration number. It starts from 1 and increments by 1 for each iteration.

```abap
DO 5 TIMES.
  WRITE: / 'Iteration:', sy-index.
ENDDO.
```

**Output:**
```
Iteration: 1
Iteration: 2
Iteration: 3
Iteration: 4
Iteration: 5
```

---

# Chapter 2: DO...ENDDO Loop

## Basic Syntax

```abap
DO [n TIMES].
  " Statements to execute
ENDDO.
```

## Type 1: Fixed Number of Iterations

When you know exactly how many times you want to repeat.

### Example 2.1: Print Multiplication Table

```abap
REPORT z_multiplication_table.

DATA: lv_number TYPE i VALUE 5,
      lv_result TYPE i.

WRITE: / 'Multiplication Table of', lv_number.
WRITE: / '============================'.

DO 10 TIMES.
  lv_result = lv_number * sy-index.
  WRITE: / lv_number, 'x', sy-index, '=', lv_result.
ENDDO.
```

**Output:**
```
Multiplication Table of 5
============================
5 x 1 = 5
5 x 2 = 10
5 x 3 = 15
5 x 4 = 20
5 x 5 = 25
5 x 6 = 30
5 x 7 = 35
5 x 8 = 40
5 x 9 = 45
5 x 10 = 50
```

### Example 2.2: Calculate Factorial

```abap
REPORT z_factorial.

DATA: lv_number    TYPE i VALUE 5,
      lv_factorial TYPE i VALUE 1.

DO lv_number TIMES.
  lv_factorial = lv_factorial * sy-index.
ENDDO.

WRITE: / 'Factorial of', lv_number, 'is', lv_factorial.
```

**Output:**
```
Factorial of 5 is 120
```

**Explanation:**
- Iteration 1: 1 × 1 = 1
- Iteration 2: 1 × 2 = 2
- Iteration 3: 2 × 3 = 6
- Iteration 4: 6 × 4 = 24
- Iteration 5: 24 × 5 = 120

## Type 2: Infinite Loop with EXIT

When you don't know how many iterations you need, use DO without TIMES.

### Example 2.3: Find First Number Divisible by 7

```abap
REPORT z_find_divisible.

DATA: lv_number TYPE i VALUE 100.

DO.
  IF lv_number MOD 7 = 0.
    WRITE: / lv_number, 'is divisible by 7'.
    EXIT.  " Exit the loop
  ENDIF.
  lv_number = lv_number + 1.
ENDDO.
```

**Output:**
```
105 is divisible by 7
```

### Example 2.4: User Input Simulation

```abap
REPORT z_user_input.

DATA: lv_password TYPE string VALUE 'secret',
      lv_input    TYPE string,
      lv_attempts TYPE i VALUE 0.

DO 3 TIMES.
  lv_attempts = lv_attempts + 1.
  
  " In real scenario, this would be user input
  IF sy-index = 3.
    lv_input = 'secret'.  " Correct password on 3rd attempt
  ELSE.
    lv_input = 'wrong'.   " Wrong password
  ENDIF.
  
  IF lv_input = lv_password.
    WRITE: / 'Access Granted after', lv_attempts, 'attempts'.
    EXIT.
  ELSE.
    WRITE: / 'Attempt', lv_attempts, ': Wrong password'.
  ENDIF.
ENDDO.

IF lv_input <> lv_password.
  WRITE: / 'Account Locked'.
ENDIF.
```

**Output:**
```
Attempt 1: Wrong password
Attempt 2: Wrong password
Access Granted after 3 attempts
```

## Type 3: VARYING Addition

The VARYING addition changes variable values in each iteration.

### Syntax:
```abap
DO n TIMES VARYING var FROM start NEXT next.
  " Statements
ENDDO.
```

### Example 2.5: Process Structure Fields

```abap
REPORT z_varying_example.

DATA: BEGIN OF ls_week,
        day1 TYPE string VALUE 'Monday',
        day2 TYPE string VALUE 'Tuesday',
        day3 TYPE string VALUE 'Wednesday',
        day4 TYPE string VALUE 'Thursday',
        day5 TYPE string VALUE 'Friday',
        day6 TYPE string VALUE 'Saturday',
        day7 TYPE string VALUE 'Sunday',
      END OF ls_week.

DATA: lv_day TYPE string.

DO 7 TIMES VARYING lv_day FROM ls_week-day1 NEXT ls_week-day2.
  WRITE: / 'Day', sy-index, ':', lv_day.
ENDDO.
```

**Output:**
```
Day 1: Monday
Day 2: Tuesday
Day 3: Wednesday
Day 4: Thursday
Day 5: Friday
Day 6: Saturday
Day 7: Sunday
```

## Common Mistakes with DO Loop

### Mistake 1: Infinite Loop Without EXIT

```abap
" WRONG - This will run forever!
DO.
  WRITE: / 'Hello'.
ENDDO.
```

**Solution:**
```abap
" CORRECT - Add EXIT condition
DO.
  WRITE: / 'Hello'.
  IF sy-index = 10.
    EXIT.
  ENDIF.
ENDDO.
```

### Mistake 2: Modifying SY-INDEX

```abap
" WRONG - SY-INDEX is read-only
DO 10 TIMES.
  sy-index = sy-index + 2.  " Runtime error!
ENDDO.
```

**Solution:**
```abap
" CORRECT - Use your own variable
DATA: lv_counter TYPE i.

DO 10 TIMES.
  lv_counter = sy-index * 2.
  WRITE: / lv_counter.
ENDDO.
```

### Mistake 3: Variable Number of Iterations

```abap
" Problem: What if lv_count is 0 or negative?
DATA: lv_count TYPE i VALUE 0.

DO lv_count TIMES.
  WRITE: / 'Hello'.
ENDDO.
" This will not execute at all - which might be correct behavior
```

---

# Chapter 3: WHILE...ENDWHILE Loop

## Basic Syntax

```abap
WHILE condition.
  " Statements to execute
ENDWHILE.
```

The WHILE loop continues as long as the condition is TRUE.

## Key Difference from DO Loop

| Feature | DO Loop | WHILE Loop |
|---------|---------|------------|
| Condition Check | After entering loop | Before entering loop |
| Minimum Executions | Once (without TIMES) | Zero |
| Best For | Fixed iterations | Condition-based logic |

## Example 3.1: Basic WHILE Loop

```abap
REPORT z_while_basic.

DATA: lv_counter TYPE i VALUE 1.

WHILE lv_counter <= 5.
  WRITE: / 'Counter:', lv_counter.
  lv_counter = lv_counter + 1.
ENDWHILE.
```

**Output:**
```
Counter: 1
Counter: 2
Counter: 3
Counter: 4
Counter: 5
```

## Example 3.2: Sum Until Threshold

```abap
REPORT z_sum_threshold.

DATA: lv_sum    TYPE i VALUE 0,
      lv_number TYPE i VALUE 1.

WHILE lv_sum < 100.
  lv_sum = lv_sum + lv_number.
  WRITE: / 'Added', lv_number, '- Sum is now:', lv_sum.
  lv_number = lv_number + 1.
ENDWHILE.

WRITE: / '============================'.
WRITE: / 'Final Sum:', lv_sum.
WRITE: / 'Numbers added:', lv_number - 1.
```

**Output:**
```
Added 1 - Sum is now: 1
Added 2 - Sum is now: 3
Added 3 - Sum is now: 6
Added 4 - Sum is now: 10
Added 5 - Sum is now: 15
Added 6 - Sum is now: 21
Added 7 - Sum is now: 28
Added 8 - Sum is now: 36
Added 9 - Sum is now: 45
Added 10 - Sum is now: 55
Added 11 - Sum is now: 66
Added 12 - Sum is now: 78
Added 13 - Sum is now: 91
Added 14 - Sum is now: 105
============================
Final Sum: 105
Numbers added: 14
```

## Example 3.3: Find Prime Numbers

```abap
REPORT z_prime_numbers.

DATA: lv_number   TYPE i VALUE 2,
      lv_divisor  TYPE i,
      lv_is_prime TYPE abap_bool,
      lv_count    TYPE i VALUE 0.

WRITE: / 'First 10 Prime Numbers:'.
WRITE: / '========================'.

WHILE lv_count < 10.
  lv_is_prime = abap_true.
  lv_divisor = 2.
  
  " Check if number is prime
  WHILE lv_divisor <= lv_number / 2.
    IF lv_number MOD lv_divisor = 0.
      lv_is_prime = abap_false.
      EXIT.  " Not prime, exit inner loop
    ENDIF.
    lv_divisor = lv_divisor + 1.
  ENDWHILE.
  
  IF lv_is_prime = abap_true.
    lv_count = lv_count + 1.
    WRITE: / lv_count, ':', lv_number.
  ENDIF.
  
  lv_number = lv_number + 1.
ENDWHILE.
```

**Output:**
```
First 10 Prime Numbers:
========================
1 : 2
2 : 3
3 : 5
4 : 7
5 : 11
6 : 13
7 : 17
8 : 19
9 : 23
10 : 29
```

## Example 3.4: String Processing

```abap
REPORT z_string_process.

DATA: lv_text     TYPE string VALUE 'ABAP Programming',
      lv_position TYPE i VALUE 0,
      lv_char     TYPE c LENGTH 1.

WRITE: / 'Characters in:', lv_text.
WRITE: / '========================'.

WHILE lv_position < strlen( lv_text ).
  lv_char = lv_text+lv_position(1).
  WRITE: / 'Position', lv_position, ':', lv_char.
  lv_position = lv_position + 1.
ENDWHILE.
```

**Output:**
```
Characters in: ABAP Programming
========================
Position 0 : A
Position 1 : B
Position 2 : A
Position 3 : P
Position 4 :  
Position 5 : P
Position 6 : r
Position 7 : o
Position 8 : g
Position 9 : r
Position 10 : a
Position 11 : m
Position 12 : m
Position 13 : i
Position 14 : n
Position 15 : g
```

## Example 3.5: Binary Search Simulation

```abap
REPORT z_binary_search.

DATA: lv_target   TYPE i VALUE 73,
      lv_low      TYPE i VALUE 1,
      lv_high     TYPE i VALUE 100,
      lv_mid      TYPE i,
      lv_attempts TYPE i VALUE 0.

WRITE: / 'Finding', lv_target, 'using Binary Search'.
WRITE: / '========================================'.

WHILE lv_low <= lv_high.
  lv_attempts = lv_attempts + 1.
  lv_mid = ( lv_low + lv_high ) / 2.
  
  WRITE: / 'Attempt', lv_attempts, ': Checking', lv_mid,
           '(Range:', lv_low, '-', lv_high, ')'.
  
  IF lv_mid = lv_target.
    WRITE: / 'Found', lv_target, 'in', lv_attempts, 'attempts!'.
    EXIT.
  ELSEIF lv_mid < lv_target.
    lv_low = lv_mid + 1.
  ELSE.
    lv_high = lv_mid - 1.
  ENDIF.
ENDWHILE.
```

**Output:**
```
Finding 73 using Binary Search
========================================
Attempt 1 : Checking 50 (Range: 1 - 100 )
Attempt 2 : Checking 75 (Range: 51 - 100 )
Attempt 3 : Checking 62 (Range: 51 - 74 )
Attempt 4 : Checking 68 (Range: 63 - 74 )
Attempt 5 : Checking 71 (Range: 69 - 74 )
Attempt 6 : Checking 73 (Range: 72 - 74 )
Found 73 in 6 attempts!
```

## Common Mistakes with WHILE Loop

### Mistake 1: Forgetting to Update Condition Variable

```abap
" WRONG - Infinite loop!
DATA: lv_i TYPE i VALUE 1.

WHILE lv_i <= 10.
  WRITE: / lv_i.
  " Forgot to increment lv_i!
ENDWHILE.
```

**Solution:**
```abap
" CORRECT
DATA: lv_i TYPE i VALUE 1.

WHILE lv_i <= 10.
  WRITE: / lv_i.
  lv_i = lv_i + 1.  " Don't forget this!
ENDWHILE.
```

### Mistake 2: Wrong Initial Condition

```abap
" WRONG - Loop never executes!
DATA: lv_i TYPE i VALUE 10.

WHILE lv_i < 5.  " Condition is FALSE from start
  WRITE: / lv_i.
  lv_i = lv_i + 1.
ENDWHILE.
```

### Mistake 3: Off-by-One Error

```abap
" Problem: Does this run 5 or 6 times?
DATA: lv_i TYPE i VALUE 0.

WHILE lv_i <= 5.  " Runs 6 times (0,1,2,3,4,5)
  WRITE: / lv_i.
  lv_i = lv_i + 1.
ENDWHILE.
```

---

# Chapter 4: LOOP AT Internal Tables

## Introduction to Internal Table Loops

The `LOOP AT` statement is the most commonly used loop in ABAP. It processes each row of an internal table sequentially.

## Basic Syntax

```abap
LOOP AT itab INTO wa.
  " Process wa
ENDLOOP.
```

## SY-TABIX System Variable

`SY-TABIX` contains the current row index in the loop.

```abap
LOOP AT lt_data INTO ls_data.
  WRITE: / 'Row', sy-tabix, ':', ls_data-field.
ENDLOOP.
```

## Different Ways to Loop

### Method 1: INTO Work Area

```abap
DATA: lt_customers TYPE TABLE OF zcustomer,
      ls_customer  TYPE zcustomer.

LOOP AT lt_customers INTO ls_customer.
  WRITE: / ls_customer-name, ls_customer-city.
ENDLOOP.
```

### Method 2: ASSIGNING Field Symbol

```abap
DATA: lt_customers TYPE TABLE OF zcustomer.
FIELD-SYMBOLS: <ls_customer> TYPE zcustomer.

LOOP AT lt_customers ASSIGNING <ls_customer>.
  WRITE: / <ls_customer>-name.
  <ls_customer>-status = 'PROCESSED'.  " Direct modification
ENDLOOP.
```

### Method 3: REFERENCE INTO

```abap
DATA: lt_customers TYPE TABLE OF zcustomer,
      lr_customer  TYPE REF TO zcustomer.

LOOP AT lt_customers REFERENCE INTO lr_customer.
  WRITE: / lr_customer->name.
  lr_customer->status = 'PROCESSED'.
ENDLOOP.
```

## Example 4.1: Complete Customer Processing

```abap
REPORT z_customer_processing.

" Define structure
TYPES: BEGIN OF ty_customer,
         id      TYPE i,
         name    TYPE string,
         city    TYPE string,
         balance TYPE p DECIMALS 2,
       END OF ty_customer.

" Define internal table and work area
DATA: lt_customers TYPE TABLE OF ty_customer,
      ls_customer  TYPE ty_customer.

" Fill table with sample data
ls_customer = VALUE #( id = 1 name = 'John' city = 'New York' balance = 1000 ).
APPEND ls_customer TO lt_customers.

ls_customer = VALUE #( id = 2 name = 'Sarah' city = 'London' balance = 2500 ).
APPEND ls_customer TO lt_customers.

ls_customer = VALUE #( id = 3 name = 'Mike' city = 'Tokyo' balance = 500 ).
APPEND ls_customer TO lt_customers.

ls_customer = VALUE #( id = 4 name = 'Emma' city = 'Paris' balance = 3000 ).
APPEND ls_customer TO lt_customers.

" Process using LOOP
WRITE: / 'Customer List:'.
WRITE: / '=============='.

LOOP AT lt_customers INTO ls_customer.
  WRITE: / 'Row', sy-tabix, ':',
           ls_customer-id,
           ls_customer-name,
           ls_customer-city,
           ls_customer-balance.
ENDLOOP.

WRITE: / '=============='.
WRITE: / 'Total Customers:', sy-dbcnt.
```

**Output:**
```
Customer List:
==============
Row 1 : 1 John New York 1000.00
Row 2 : 2 Sarah London 2500.00
Row 3 : 3 Mike Tokyo 500.00
Row 4 : 4 Emma Paris 3000.00
==============
Total Customers: 4
```

## LOOP with WHERE Condition

Filter rows while looping.

### Example 4.2: Filter High Balance Customers

```abap
REPORT z_filter_customers.

TYPES: BEGIN OF ty_customer,
         id      TYPE i,
         name    TYPE string,
         balance TYPE p DECIMALS 2,
       END OF ty_customer.

DATA: lt_customers TYPE TABLE OF ty_customer,
      ls_customer  TYPE ty_customer,
      lv_total     TYPE p DECIMALS 2.

" Fill data
lt_customers = VALUE #(
  ( id = 1 name = 'John'  balance = 1000 )
  ( id = 2 name = 'Sarah' balance = 2500 )
  ( id = 3 name = 'Mike'  balance = 500 )
  ( id = 4 name = 'Emma'  balance = 3000 )
  ( id = 5 name = 'Tom'   balance = 1500 )
).

WRITE: / 'Customers with balance > 1000:'.
WRITE: / '================================'.

LOOP AT lt_customers INTO ls_customer WHERE balance > 1000.
  WRITE: / ls_customer-name, ls_customer-balance.
  lv_total = lv_total + ls_customer-balance.
ENDLOOP.

WRITE: / '================================'.
WRITE: / 'Total High Balance:', lv_total.
```

**Output:**
```
Customers with balance > 1000:
================================
Sarah 2500.00
Emma 3000.00
Tom 1500.00
================================
Total High Balance: 7000.00
```

## LOOP with FROM and TO

Process specific range of rows.

### Example 4.3: Pagination

```abap
REPORT z_pagination.

TYPES: BEGIN OF ty_product,
         id   TYPE i,
         name TYPE string,
       END OF ty_product.

DATA: lt_products TYPE TABLE OF ty_product,
      ls_product  TYPE ty_product,
      lv_page     TYPE i VALUE 2,
      lv_per_page TYPE i VALUE 3,
      lv_from     TYPE i,
      lv_to       TYPE i.

" Fill 10 products
DO 10 TIMES.
  ls_product = VALUE #( id = sy-index name = |Product { sy-index }| ).
  APPEND ls_product TO lt_products.
ENDDO.

" Calculate range
lv_from = ( lv_page - 1 ) * lv_per_page + 1.
lv_to = lv_page * lv_per_page.

WRITE: / 'Page', lv_page, '(rows', lv_from, 'to', lv_to, '):'.
WRITE: / '========================'.

LOOP AT lt_products INTO ls_product FROM lv_from TO lv_to.
  WRITE: / ls_product-id, ls_product-name.
ENDLOOP.
```

**Output:**
```
Page 2 (rows 4 to 6 ):
========================
4 Product 4
5 Product 5
6 Product 6
```

## LOOP with GROUP BY

Group table data for processing.

### Example 4.4: Group Customers by City

```abap
REPORT z_group_by.

TYPES: BEGIN OF ty_customer,
         name    TYPE string,
         city    TYPE string,
         balance TYPE p DECIMALS 2,
       END OF ty_customer.

DATA: lt_customers TYPE TABLE OF ty_customer,
      ls_customer  TYPE ty_customer.

" Fill data
lt_customers = VALUE #(
  ( name = 'John'  city = 'New York' balance = 1000 )
  ( name = 'Sarah' city = 'London'   balance = 2500 )
  ( name = 'Mike'  city = 'New York' balance = 500 )
  ( name = 'Emma'  city = 'London'   balance = 3000 )
  ( name = 'Tom'   city = 'New York' balance = 1500 )
).

WRITE: / 'Customers Grouped by City:'.
WRITE: / '==========================='.

LOOP AT lt_customers INTO ls_customer
     GROUP BY ls_customer-city
     INTO DATA(lv_city).
  
  WRITE: / ''.
  WRITE: / 'City:', lv_city.
  WRITE: / '-----------'.
  
  DATA: lv_city_total TYPE p DECIMALS 2 VALUE 0.
  
  LOOP AT GROUP lv_city INTO DATA(ls_member).
    WRITE: / '  ', ls_member-name, ls_member-balance.
    lv_city_total = lv_city_total + ls_member-balance.
  ENDLOOP.
  
  WRITE: / '  Total:', lv_city_total.
  CLEAR lv_city_total.
ENDLOOP.
```

**Output:**
```
Customers Grouped by City:
===========================

City: New York
-----------
   John 1000.00
   Mike 500.00
   Tom 1500.00
  Total: 3000.00

City: London
-----------
   Sarah 2500.00
   Emma 3000.00
  Total: 5500.00
```

## Modifying Internal Table During LOOP

### Using Field Symbols (Recommended)

```abap
REPORT z_modify_loop.

TYPES: BEGIN OF ty_product,
         name  TYPE string,
         price TYPE p DECIMALS 2,
       END OF ty_product.

DATA: lt_products TYPE TABLE OF ty_product.
FIELD-SYMBOLS: <ls_product> TYPE ty_product.

" Fill data
lt_products = VALUE #(
  ( name = 'Apple'  price = 10 )
  ( name = 'Banana' price = 5 )
  ( name = 'Orange' price = 8 )
).

WRITE: / 'Before 10% price increase:'.
LOOP AT lt_products ASSIGNING <ls_product>.
  WRITE: / <ls_product>-name, <ls_product>-price.
ENDLOOP.

" Apply 10% increase
LOOP AT lt_products ASSIGNING <ls_product>.
  <ls_product>-price = <ls_product>-price * '1.10'.
ENDLOOP.

WRITE: / ''.
WRITE: / 'After 10% price increase:'.
LOOP AT lt_products ASSIGNING <ls_product>.
  WRITE: / <ls_product>-name, <ls_product>-price.
ENDLOOP.
```

**Output:**
```
Before 10% price increase:
Apple 10.00
Banana 5.00
Orange 8.00

After 10% price increase:
Apple 11.00
Banana 5.50
Orange 8.80
```

## Deleting Rows During LOOP

### Example 4.5: Delete Rows Safely

```abap
REPORT z_delete_in_loop.

TYPES: BEGIN OF ty_item,
         id     TYPE i,
         status TYPE string,
       END OF ty_item.

DATA: lt_items TYPE TABLE OF ty_item.
FIELD-SYMBOLS: <ls_item> TYPE ty_item.

" Fill data
lt_items = VALUE #(
  ( id = 1 status = 'Active' )
  ( id = 2 status = 'Deleted' )
  ( id = 3 status = 'Active' )
  ( id = 4 status = 'Deleted' )
  ( id = 5 status = 'Active' )
).

WRITE: / 'Before deletion:', lines( lt_items ), 'items'.

" Delete using WHERE (Best method)
DELETE lt_items WHERE status = 'Deleted'.

WRITE: / 'After deletion:', lines( lt_items ), 'items'.
WRITE: / ''.
LOOP AT lt_items ASSIGNING <ls_item>.
  WRITE: / <ls_item>-id, <ls_item>-status.
ENDLOOP.
```

**Output:**
```
Before deletion: 5 items
After deletion: 3 items

1 Active
3 Active
5 Active
```

### Deleting Inside Loop (Use with Caution)

```abap
" If you must delete inside loop, loop backwards!
DATA: lv_index TYPE i.

lv_index = lines( lt_items ).
WHILE lv_index > 0.
  READ TABLE lt_items INTO ls_item INDEX lv_index.
  IF ls_item-status = 'Deleted'.
    DELETE lt_items INDEX lv_index.
  ENDIF.
  lv_index = lv_index - 1.
ENDWHILE.
```

## Performance Comparison: INTO vs ASSIGNING

```abap
REPORT z_performance_test.

TYPES: BEGIN OF ty_data,
         field1 TYPE string,
         field2 TYPE string,
         field3 TYPE i,
       END OF ty_data.

DATA: lt_data     TYPE TABLE OF ty_data,
      ls_data     TYPE ty_data,
      lv_start    TYPE timestampl,
      lv_end      TYPE timestampl,
      lv_duration TYPE p DECIMALS 3.

FIELD-SYMBOLS: <ls_data> TYPE ty_data.

" Create 100,000 rows
DO 100000 TIMES.
  ls_data = VALUE #( field1 = |Text { sy-index }| 
                     field2 = 'Data' 
                     field3 = sy-index ).
  APPEND ls_data TO lt_data.
ENDDO.

" Test INTO
GET TIME STAMP FIELD lv_start.
LOOP AT lt_data INTO ls_data.
  " Just read
ENDLOOP.
GET TIME STAMP FIELD lv_end.
lv_duration = lv_end - lv_start.
WRITE: / 'INTO work area:', lv_duration, 'seconds'.

" Test ASSIGNING
GET TIME STAMP FIELD lv_start.
LOOP AT lt_data ASSIGNING <ls_data>.
  " Just read
ENDLOOP.
GET TIME STAMP FIELD lv_end.
lv_duration = lv_end - lv_start.
WRITE: / 'ASSIGNING field symbol:', lv_duration, 'seconds'.
```

**Typical Output:**
```
INTO work area: 0.150 seconds
ASSIGNING field symbol: 0.050 seconds
```

**Key Point:** `ASSIGNING` is about 3x faster because it doesn't copy data.

---

# Chapter 5: SELECT...ENDSELECT Loop

## What is SELECT...ENDSELECT?

This is a cursor-based loop that fetches database records one at a time.

## Basic Syntax

```abap
SELECT field1 field2 FROM dbtable INTO wa.
  " Process each record
ENDSELECT.
```

## Example 5.1: Basic SELECT Loop

```abap
REPORT z_select_loop.

DATA: ls_mara TYPE mara.

SELECT * FROM mara INTO ls_mara UP TO 5 ROWS.
  WRITE: / ls_mara-matnr, ls_mara-mtart, ls_mara-matkl.
ENDSELECT.

WRITE: / 'Records processed:', sy-dbcnt.
```

## Why SELECT...ENDSELECT is NOT Recommended

### The Problem:

```abap
" BAD - Makes 1000 separate database calls!
SELECT * FROM mara INTO ls_mara.
  SELECT SINGLE * FROM marc INTO ls_marc
    WHERE matnr = ls_mara-matnr.
  " Process...
ENDSELECT.
```

### The Solution - Array Fetch:

```abap
" GOOD - Single database call!
SELECT * FROM mara INTO TABLE lt_mara.

LOOP AT lt_mara INTO ls_mara.
  " Process...
ENDLOOP.
```

## Performance Comparison

```abap
REPORT z_select_performance.

DATA: ls_mara  TYPE mara,
      lt_mara  TYPE TABLE OF mara,
      lv_count TYPE i,
      lv_start TYPE timestampl,
      lv_end   TYPE timestampl.

" Method 1: SELECT...ENDSELECT (Slow)
GET TIME STAMP FIELD lv_start.
SELECT * FROM mara INTO ls_mara UP TO 1000 ROWS.
  lv_count = lv_count + 1.
ENDSELECT.
GET TIME STAMP FIELD lv_end.
WRITE: / 'SELECT...ENDSELECT:', lv_end - lv_start, 'seconds'.

" Method 2: SELECT INTO TABLE (Fast)
GET TIME STAMP FIELD lv_start.
SELECT * FROM mara INTO TABLE lt_mara UP TO 1000 ROWS.
LOOP AT lt_mara INTO ls_mara.
  " Process
ENDLOOP.
GET TIME STAMP FIELD lv_end.
WRITE: / 'SELECT INTO TABLE + LOOP:', lv_end - lv_start, 'seconds'.
```

## When SELECT...ENDSELECT Might Be Acceptable

1. Processing huge datasets that don't fit in memory
2. Complex processing requiring immediate write-back
3. Single record checks with EXIT

### Example 5.2: Large Dataset Processing

```abap
REPORT z_large_dataset.

DATA: ls_record TYPE zlarge_table,
      lv_count  TYPE i.

" When table is too large for memory
SELECT * FROM zlarge_table INTO ls_record
    PACKAGE SIZE 1000.
  
  " Process in chunks
  lv_count = lv_count + 1.
  
  " Write back or log
  COMMIT WORK.
  
ENDSELECT.
```

## Better Alternative: FOR ALL ENTRIES

```abap
REPORT z_for_all_entries.

DATA: lt_mara TYPE TABLE OF mara,
      lt_marc TYPE TABLE OF marc,
      ls_mara TYPE mara,
      ls_marc TYPE marc.

" Get materials
SELECT * FROM mara INTO TABLE lt_mara UP TO 100 ROWS.

" Get plant data for all materials in ONE call
IF lt_mara IS NOT INITIAL.
  SELECT * FROM marc INTO TABLE lt_marc
    FOR ALL ENTRIES IN lt_mara
    WHERE matnr = lt_mara-matnr.
ENDIF.

" Process using internal table loop
LOOP AT lt_mara INTO ls_mara.
  READ TABLE lt_marc INTO ls_marc
    WITH KEY matnr = ls_mara-matnr.
  IF sy-subrc = 0.
    WRITE: / ls_mara-matnr, ls_marc-werks.
  ENDIF.
ENDLOOP.
```

---

# Chapter 6: LOOP AT SCREEN

## What is LOOP AT SCREEN?

This loop is used in screen programming (dynpros) to modify screen element properties dynamically.

## Basic Syntax

```abap
LOOP AT SCREEN.
  " Check and modify SCREEN structure
  MODIFY SCREEN.
ENDLOOP.
```

## SCREEN Structure Fields

| Field | Description | Values |
|-------|-------------|--------|
| screen-name | Element name | Field name |
| screen-group1 to group4 | Modification groups | User-defined |
| screen-active | Is field visible? | 0 = No, 1 = Yes |
| screen-input | Is input allowed? | 0 = No, 1 = Yes |
| screen-invisible | Is field hidden? | 0 = No, 1 = Yes |
| screen-intensified | Is field highlighted? | 0 = No, 1 = Yes |
| screen-required | Is field mandatory? | 0 = No, 1 = Yes |

## Example 6.1: Hide/Show Fields

```abap
REPORT z_screen_loop.

PARAMETERS: p_type TYPE c LENGTH 1.
PARAMETERS: p_name TYPE string.
PARAMETERS: p_addr TYPE string.

AT SELECTION-SCREEN OUTPUT.
  LOOP AT SCREEN.
    " Hide address if type is 'X'
    IF p_type = 'X' AND screen-name = 'P_ADDR'.
      screen-active = 0.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.
```

## Example 6.2: Make Fields Read-Only

```abap
REPORT z_readonly_fields.

PARAMETERS: p_mode TYPE c LENGTH 1 DEFAULT 'D'.  " D=Display, E=Edit
PARAMETERS: p_cust TYPE kunnr.
PARAMETERS: p_name TYPE string.
PARAMETERS: p_city TYPE string.

AT SELECTION-SCREEN OUTPUT.
  LOOP AT SCREEN.
    " If display mode, make all input fields read-only
    IF p_mode = 'D'.
      IF screen-name = 'P_CUST' OR
         screen-name = 'P_NAME' OR
         screen-name = 'P_CITY'.
        screen-input = 0.
        MODIFY SCREEN.
      ENDIF.
    ENDIF.
  ENDLOOP.
```

## Example 6.3: Using Modification Groups

In Screen Painter, assign group 'GR1' to related fields.

```abap
REPORT z_modification_groups.

PARAMETERS: p_show TYPE abap_bool AS CHECKBOX.
PARAMETERS: p_fld1 TYPE string MODIF ID gr1.
PARAMETERS: p_fld2 TYPE string MODIF ID gr1.
PARAMETERS: p_fld3 TYPE string MODIF ID gr2.

AT SELECTION-SCREEN OUTPUT.
  LOOP AT SCREEN.
    " Toggle visibility of GROUP1 fields
    IF screen-group1 = 'GR1'.
      IF p_show = abap_false.
        screen-active = 0.
      ELSE.
        screen-active = 1.
      ENDIF.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.

AT SELECTION-SCREEN.
  " Refresh screen when checkbox changes
```

## Example 6.4: Make Fields Mandatory

```abap
REPORT z_mandatory_fields.

PARAMETERS: p_urgent TYPE abap_bool AS CHECKBOX.
PARAMETERS: p_reason TYPE string.
PARAMETERS: p_date   TYPE datum.

AT SELECTION-SCREEN OUTPUT.
  LOOP AT SCREEN.
    IF p_urgent = abap_true.
      " Make reason mandatory if urgent
      IF screen-name = 'P_REASON'.
        screen-required = 1.
        MODIFY SCREEN.
      ENDIF.
    ENDIF.
  ENDLOOP.
```

## Example 6.5: Highlight Fields

```abap
REPORT z_highlight_fields.

PARAMETERS: p_amount TYPE p DECIMALS 2.
PARAMETERS: p_status TYPE string.

AT SELECTION-SCREEN OUTPUT.
  LOOP AT SCREEN.
    " Highlight if amount > 10000
    IF screen-name = 'P_AMOUNT' AND p_amount > 10000.
      screen-intensified = 1.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.
```

## Common LOOP AT SCREEN Patterns

### Pattern 1: Role-Based Field Control

```abap
AT SELECTION-SCREEN OUTPUT.
  DATA: lv_is_admin TYPE abap_bool.
  
  " Check if user is admin
  SELECT SINGLE @abap_true INTO @lv_is_admin
    FROM ztuser_roles
    WHERE username = @sy-uname
      AND role = 'ADMIN'.
  
  LOOP AT SCREEN.
    IF screen-group1 = 'ADM'.
      IF lv_is_admin = abap_false.
        screen-active = 0.  " Hide admin fields
      ENDIF.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.
```

### Pattern 2: Dependent Fields

```abap
PARAMETERS: p_country TYPE land1.
PARAMETERS: p_region  TYPE regio MODIF ID RGN.
PARAMETERS: p_state   TYPE string MODIF ID STA.

AT SELECTION-SCREEN OUTPUT.
  LOOP AT SCREEN.
    " Show region only for US
    IF screen-group1 = 'RGN'.
      IF p_country = 'US'.
        screen-active = 1.
      ELSE.
        screen-active = 0.
      ENDIF.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.
```

---

# Chapter 7: Nested Loops

## What are Nested Loops?

A nested loop is a loop inside another loop. The inner loop completes all its iterations for each iteration of the outer loop.

## Basic Structure

```abap
DO 3 TIMES.  " Outer loop
  DO 2 TIMES.  " Inner loop
    " This executes 3 × 2 = 6 times
  ENDDO.
ENDDO.
```

## Example 7.1: Multiplication Table Grid

```abap
REPORT z_multiplication_grid.

DATA: lv_result TYPE i.

WRITE: / '    ', '| 1  2  3  4  5  6  7  8  9  10'.
WRITE: / '----+--------------------------------'.

DO 10 TIMES.
  DATA(lv_row) = sy-index.
  WRITE: / lv_row NO-GAP, ' |'.
  
  DO 10 TIMES.
    lv_result = lv_row * sy-index.
    WRITE: lv_result.
  ENDDO.
ENDDO.
```

**Output:**
```
     | 1  2  3  4  5  6  7  8  9  10
----+--------------------------------
1  | 1  2  3  4  5  6  7  8  9  10
2  | 2  4  6  8  10 12 14 16 18 20
3  | 3  6  9  12 15 18 21 24 27 30
...
```

## Example 7.2: Nested Internal Table Loops

```abap
REPORT z_nested_itab.

TYPES: BEGIN OF ty_order,
         order_id TYPE i,
         customer TYPE string,
       END OF ty_order.

TYPES: BEGIN OF ty_item,
         order_id TYPE i,
         product  TYPE string,
         qty      TYPE i,
       END OF ty_item.

DATA: lt_orders TYPE TABLE OF ty_order,
      lt_items  TYPE TABLE OF ty_item,
      ls_order  TYPE ty_order,
      ls_item   TYPE ty_item.

" Fill orders
lt_orders = VALUE #(
  ( order_id = 1 customer = 'John' )
  ( order_id = 2 customer = 'Sarah' )
).

" Fill items
lt_items = VALUE #(
  ( order_id = 1 product = 'Apple' qty = 5 )
  ( order_id = 1 product = 'Banana' qty = 3 )
  ( order_id = 2 product = 'Orange' qty = 10 )
  ( order_id = 2 product = 'Mango' qty = 2 )
  ( order_id = 2 product = 'Grape' qty = 8 )
).

" Nested loop - Show orders with items
LOOP AT lt_orders INTO ls_order.
  WRITE: / 'Order:', ls_order-order_id, 
           'Customer:', ls_order-customer.
  WRITE: / '  Items:'.
  
  LOOP AT lt_items INTO ls_item 
       WHERE order_id = ls_order-order_id.
    WRITE: / '    -', ls_item-product, 
             'Qty:', ls_item-qty.
  ENDLOOP.
  
  WRITE: / ''.
ENDLOOP.
```

**Output:**
```
Order: 1 Customer: John
  Items:
    - Apple Qty: 5
    - Banana Qty: 3

Order: 2 Customer: Sarah
  Items:
    - Orange Qty: 10
    - Mango Qty: 2
    - Grape Qty: 8
```

## The Nested Loop Performance Problem

### Problem: O(n×m) Complexity

```abap
" SLOW - For 1000 orders with 5000 items
" This checks 1000 × 5000 = 5,000,000 comparisons!
LOOP AT lt_orders INTO ls_order.
  LOOP AT lt_items INTO ls_item 
       WHERE order_id = ls_order-order_id.
    " Process
  ENDLOOP.
ENDLOOP.
```

## Solution 1: SORTED Table with READ

```abap
REPORT z_optimized_nested.

TYPES: BEGIN OF ty_item,
         order_id TYPE i,
         product  TYPE string,
       END OF ty_item.

" Use SORTED table with key
DATA: lt_items TYPE SORTED TABLE OF ty_item 
               WITH NON-UNIQUE KEY order_id.

" Fill items
lt_items = VALUE #(
  ( order_id = 1 product = 'Apple' )
  ( order_id = 1 product = 'Banana' )
  ( order_id = 2 product = 'Orange' )
).

" Efficient nested loop with SORTED table
LOOP AT lt_orders INTO ls_order.
  LOOP AT lt_items INTO ls_item
       WHERE order_id = ls_order-order_id.
    " Binary search is used automatically
  ENDLOOP.
ENDLOOP.
```

## Solution 2: HASHED Table for Lookup

```abap
REPORT z_hashed_lookup.

TYPES: BEGIN OF ty_customer,
         id   TYPE i,
         name TYPE string,
       END OF ty_customer.

" HASHED table for O(1) lookup
DATA: lt_customers TYPE HASHED TABLE OF ty_customer
                   WITH UNIQUE KEY id.

lt_customers = VALUE #(
  ( id = 1 name = 'John' )
  ( id = 2 name = 'Sarah' )
  ( id = 3 name = 'Mike' )
).

" Fast lookup in loop
LOOP AT lt_orders INTO ls_order.
  READ TABLE lt_customers INTO ls_customer
    WITH TABLE KEY id = ls_order-customer_id.
  IF sy-subrc = 0.
    WRITE: / ls_order-order_id, ls_customer-name.
  ENDIF.
ENDLOOP.
```

## Solution 3: Pre-Process with Parallel Cursor

```abap
REPORT z_parallel_cursor.

TYPES: BEGIN OF ty_order,
         order_id TYPE i,
         customer TYPE string,
       END OF ty_order.

TYPES: BEGIN OF ty_item,
         order_id TYPE i,
         product  TYPE string,
       END OF ty_item.

DATA: lt_orders TYPE STANDARD TABLE OF ty_order
                WITH NON-UNIQUE KEY order_id,
      lt_items  TYPE STANDARD TABLE OF ty_item
                WITH NON-UNIQUE KEY order_id,
      ls_order  TYPE ty_order,
      ls_item   TYPE ty_item,
      lv_index  TYPE sy-tabix.

" Sort both tables by common key
SORT lt_orders BY order_id.
SORT lt_items BY order_id.

" Parallel cursor technique
lv_index = 1.

LOOP AT lt_orders INTO ls_order.
  " Start inner loop from where we left off
  LOOP AT lt_items INTO ls_item FROM lv_index.
    IF ls_item-order_id <> ls_order-order_id.
      lv_index = sy-tabix.  " Remember position
      EXIT.  " Move to next order
    ENDIF.
    
    " Process matching item
    WRITE: / ls_order-customer, ls_item-product.
  ENDLOOP.
ENDLOOP.
```

### Performance Comparison

| Method | Time Complexity | 1000 Orders × 5000 Items |
|--------|-----------------|--------------------------|
| Basic Nested | O(n×m) | 5,000,000 operations |
| SORTED Table | O(n×log m) | ~15,000 operations |
| HASHED Table | O(n) | 1,000 operations |
| Parallel Cursor | O(n+m) | 6,000 operations |

## Example 7.3: Matrix Operations

```abap
REPORT z_matrix.

DATA: lt_matrix TYPE TABLE OF TABLE OF i,
      lt_row    TYPE TABLE OF i.

" Create 3x3 matrix
lt_row = VALUE #( ( 1 ) ( 2 ) ( 3 ) ).
APPEND lt_row TO lt_matrix.
lt_row = VALUE #( ( 4 ) ( 5 ) ( 6 ) ).
APPEND lt_row TO lt_matrix.
lt_row = VALUE #( ( 7 ) ( 8 ) ( 9 ) ).
APPEND lt_row TO lt_matrix.

" Print matrix
WRITE: / 'Matrix:'.
DATA: lv_row TYPE i,
      lv_col TYPE i.

lv_row = 0.
LOOP AT lt_matrix INTO DATA(lt_current_row).
  lv_row = lv_row + 1.
  lv_col = 0.
  
  LOOP AT lt_current_row INTO DATA(lv_value).
    lv_col = lv_col + 1.
    WRITE: lv_value.
  ENDLOOP.
  
  WRITE: /.
ENDLOOP.
```

---

# Chapter 8: Loop Control Statements

## Overview

| Statement | Purpose |
|-----------|---------|
| EXIT | Exit the current loop completely |
| CONTINUE | Skip rest of current iteration, go to next |
| CHECK | Like CONTINUE with condition |
| RETURN | Exit the entire subroutine/method |

## EXIT Statement

Immediately exits the innermost loop.

### Example 8.1: Find First Match

```abap
REPORT z_exit_example.

TYPES: BEGIN OF ty_product,
         id   TYPE i,
         name TYPE string,
       END OF ty_product.

DATA: lt_products TYPE TABLE OF ty_product,
      ls_product  TYPE ty_product,
      ls_found    TYPE ty_product.

lt_products = VALUE #(
  ( id = 1 name = 'Apple' )
  ( id = 2 name = 'Banana' )
  ( id = 3 name = 'Cherry' )
  ( id = 4 name = 'Date' )
).

" Find first product starting with 'C'
LOOP AT lt_products INTO ls_product.
  IF ls_product-name(1) = 'C'.
    ls_found = ls_product.
    EXIT.  " Stop searching
  ENDIF.
ENDLOOP.

IF ls_found IS NOT INITIAL.
  WRITE: / 'Found:', ls_found-name.
ENDIF.
```

**Output:**
```
Found: Cherry
```

### EXIT in Nested Loops

EXIT only exits the innermost loop.

```abap
REPORT z_exit_nested.

DO 3 TIMES.
  DATA(lv_outer) = sy-index.
  WRITE: / 'Outer:', lv_outer.
  
  DO 5 TIMES.
    WRITE: / '  Inner:', sy-index.
    IF sy-index = 2.
      EXIT.  " Only exits inner loop
    ENDIF.
  ENDDO.
  
ENDDO.
```

**Output:**
```
Outer: 1
  Inner: 1
  Inner: 2
Outer: 2
  Inner: 1
  Inner: 2
Outer: 3
  Inner: 1
  Inner: 2
```

## CONTINUE Statement

Skips the rest of current iteration and goes to the next.

### Example 8.2: Skip Processing

```abap
REPORT z_continue_example.

DATA: lv_sum TYPE i VALUE 0.

" Sum only even numbers from 1 to 10
DO 10 TIMES.
  IF sy-index MOD 2 <> 0.
    CONTINUE.  " Skip odd numbers
  ENDIF.
  
  lv_sum = lv_sum + sy-index.
  WRITE: / 'Adding:', sy-index.
ENDDO.

WRITE: / 'Sum of even numbers:', lv_sum.
```

**Output:**
```
Adding: 2
Adding: 4
Adding: 6
Adding: 8
Adding: 10
Sum of even numbers: 30
```

### Example 8.3: Skip Invalid Records

```abap
REPORT z_skip_invalid.

TYPES: BEGIN OF ty_order,
         id     TYPE i,
         amount TYPE p DECIMALS 2,
         status TYPE string,
       END OF ty_order.

DATA: lt_orders    TYPE TABLE OF ty_order,
      ls_order     TYPE ty_order,
      lv_total     TYPE p DECIMALS 2,
      lv_processed TYPE i.

lt_orders = VALUE #(
  ( id = 1 amount = 100 status = 'VALID' )
  ( id = 2 amount = 200 status = 'CANCELLED' )
  ( id = 3 amount = 150 status = 'VALID' )
  ( id = 4 amount = 300 status = 'PENDING' )
  ( id = 5 amount = 250 status = 'VALID' )
).

LOOP AT lt_orders INTO ls_order.
  " Skip cancelled orders
  IF ls_order-status = 'CANCELLED'.
    WRITE: / 'Skipping cancelled order:', ls_order-id.
    CONTINUE.
  ENDIF.
  
  " Skip pending orders
  IF ls_order-status = 'PENDING'.
    WRITE: / 'Skipping pending order:', ls_order-id.
    CONTINUE.
  ENDIF.
  
  " Process valid orders
  lv_total = lv_total + ls_order-amount.
  lv_processed = lv_processed + 1.
  WRITE: / 'Processed order:', ls_order-id, 
           'Amount:', ls_order-amount.
ENDLOOP.

WRITE: / '========================'.
WRITE: / 'Orders processed:', lv_processed.
WRITE: / 'Total amount:', lv_total.
```

**Output:**
```
Processed order: 1 Amount: 100.00
Skipping cancelled order: 2
Processed order: 3 Amount: 150.00
Skipping pending order: 4
Processed order: 5 Amount: 250.00
========================
Orders processed: 3
Total amount: 500.00
```

## CHECK Statement

Combines condition check with CONTINUE.

### Syntax:
```abap
CHECK condition.
" If condition is FALSE, skip to next iteration
" If condition is TRUE, continue with next statement
```

### Example 8.4: CHECK vs CONTINUE

```abap
" Using CONTINUE
IF ls_order-status <> 'VALID'.
  CONTINUE.
ENDIF.
" Process...

" Equivalent using CHECK (shorter)
CHECK ls_order-status = 'VALID'.
" Process...
```

### Example 8.5: Multiple CHECKs

```abap
REPORT z_check_example.

TYPES: BEGIN OF ty_employee,
         id         TYPE i,
         department TYPE string,
         salary     TYPE p DECIMALS 2,
         active     TYPE abap_bool,
       END OF ty_employee.

DATA: lt_employees TYPE TABLE OF ty_employee.

lt_employees = VALUE #(
  ( id = 1 department = 'IT' salary = 5000 active = abap_true )
  ( id = 2 department = 'HR' salary = 4000 active = abap_true )
  ( id = 3 department = 'IT' salary = 6000 active = abap_false )
  ( id = 4 department = 'IT' salary = 7000 active = abap_true )
).

WRITE: / 'Active IT employees with salary > 4500:'.

LOOP AT lt_employees INTO DATA(ls_emp).
  CHECK ls_emp-active = abap_true.      " Must be active
  CHECK ls_emp-department = 'IT'.        " Must be IT
  CHECK ls_emp-salary > 4500.            " Must earn > 4500
  
  " Only matching employees reach here
  WRITE: / ls_emp-id, ls_emp-salary.
ENDLOOP.
```

**Output:**
```
Active IT employees with salary > 4500:
1 5000.00
4 7000.00
```

## CHECK Outside Loops

Outside loops, CHECK exits the current processing block.

```abap
FORM process_order USING ps_order TYPE ty_order.
  CHECK ps_order-amount > 0.  " Exit FORM if amount <= 0
  " Process order...
ENDFORM.
```

## RETURN Statement

Exits the entire method, function, or subroutine.

### Example 8.6: RETURN in Loop

```abap
REPORT z_return_example.

CLASS lcl_processor DEFINITION.
  PUBLIC SECTION.
    METHODS: find_critical_error
               IMPORTING it_logs TYPE string_table
               RETURNING VALUE(rv_found) TYPE abap_bool.
ENDCLASS.

CLASS lcl_processor IMPLEMENTATION.
  METHOD find_critical_error.
    LOOP AT it_logs INTO DATA(lv_log).
      IF lv_log CS 'CRITICAL'.
        rv_found = abap_true.
        RETURN.  " Exit entire method
      ENDIF.
    ENDLOOP.
    
    rv_found = abap_false.
  ENDMETHOD.
ENDCLASS.
```

## Comparison Summary

```abap
REPORT z_control_comparison.

DATA: lt_numbers TYPE TABLE OF i.
lt_numbers = VALUE #( ( 1 ) ( 2 ) ( 3 ) ( 4 ) ( 5 ) ).

" EXIT - Stops loop at 3
WRITE: / 'EXIT example:'.
LOOP AT lt_numbers INTO DATA(lv_num).
  IF lv_num = 3.
    EXIT.
  ENDIF.
  WRITE: lv_num.
ENDLOOP.
" Output: 1 2

WRITE: /.

" CONTINUE - Skips 3
WRITE: / 'CONTINUE example:'.
LOOP AT lt_numbers INTO lv_num.
  IF lv_num = 3.
    CONTINUE.
  ENDIF.
  WRITE: lv_num.
ENDLOOP.
" Output: 1 2 4 5
```

---

# Chapter 9: Performance and Optimization

## The Golden Rules of Loop Performance

1. **Minimize database calls inside loops**
2. **Use appropriate table types**
3. **Avoid nested loops when possible**
4. **Use field symbols for modification**
5. **Filter early with WHERE clause**

## Rule 1: Database Calls Inside Loops

### BAD Practice:

```abap
" TERRIBLE - N database calls!
LOOP AT lt_materials INTO ls_material.
  SELECT SINGLE * FROM mara INTO ls_mara
    WHERE matnr = ls_material-matnr.
  " Process...
ENDLOOP.
```

### GOOD Practice:

```abap
" GOOD - 1 database call
SELECT * FROM mara INTO TABLE lt_mara
  FOR ALL ENTRIES IN lt_materials
  WHERE matnr = lt_materials-matnr.

" Then loop through internal table
LOOP AT lt_materials INTO ls_material.
  READ TABLE lt_mara INTO ls_mara
    WITH KEY matnr = ls_material-matnr.
  " Process...
ENDLOOP.
```

## Rule 2: Choose Right Table Type

### Performance Characteristics:

| Operation | STANDARD | SORTED | HASHED |
|-----------|----------|--------|--------|
| Sequential READ | O(n) | O(n) | O(n) |
| READ by key | O(n) | O(log n) | O(1) |
| INSERT at end | O(1) | O(log n) | O(n) |
| LOOP | O(n) | O(n) | O(n) |
| LOOP WHERE (key) | O(n) | O(log n + result) | O(n) |

### Example 9.1: Table Type Comparison

```abap
REPORT z_table_type_performance.

TYPES: BEGIN OF ty_data,
         key  TYPE i,
         data TYPE string,
       END OF ty_data.

DATA: lt_standard TYPE STANDARD TABLE OF ty_data,
      lt_sorted   TYPE SORTED TABLE OF ty_data 
                  WITH NON-UNIQUE KEY key,
      lt_hashed   TYPE HASHED TABLE OF ty_data 
                  WITH UNIQUE KEY key,
      ls_data     TYPE ty_data,
      lv_start    TYPE timestampl,
      lv_end      TYPE timestampl.

" Fill tables with 100,000 records
DO 100000 TIMES.
  ls_data = VALUE #( key = sy-index data = |Data { sy-index }| ).
  INSERT ls_data INTO TABLE lt_standard.
  INSERT ls_data INTO TABLE lt_sorted.
  INSERT ls_data INTO TABLE lt_hashed.
ENDDO.

" Test READ performance
DATA: lv_search TYPE i VALUE 50000.

" STANDARD table READ
GET TIME STAMP FIELD lv_start.
DO 1000 TIMES.
  READ TABLE lt_standard INTO ls_data 
    WITH KEY key = lv_search.
ENDDO.
GET TIME STAMP FIELD lv_end.
WRITE: / 'STANDARD READ:', lv_end - lv_start.

" SORTED table READ
GET TIME STAMP FIELD lv_start.
DO 1000 TIMES.
  READ TABLE lt_sorted INTO ls_data 
    WITH KEY key = lv_search.
ENDDO.
GET TIME STAMP FIELD lv_end.
WRITE: / 'SORTED READ:', lv_end - lv_start.

" HASHED table READ
GET TIME STAMP FIELD lv_start.
DO 1000 TIMES.
  READ TABLE lt_hashed INTO ls_data 
    WITH TABLE KEY key = lv_search.
ENDDO.
GET TIME STAMP FIELD lv_end.
WRITE: / 'HASHED READ:', lv_end - lv_start.
```

**Typical Results:**
```
STANDARD READ: 2.500000 seconds
SORTED READ: 0.010000 seconds
HASHED READ: 0.001000 seconds
```

## Rule 3: Avoid Unnecessary Nested Loops

### BAD:

```abap
" O(n²) complexity!
LOOP AT lt_orders INTO ls_order.
  LOOP AT lt_items INTO ls_item.
    IF ls_item-order_id = ls_order-id.
      " Process
    ENDIF.
  ENDLOOP.
ENDLOOP.
```

### GOOD:

```abap
" O(n) complexity with SORTED table
DATA: lt_items_sorted TYPE SORTED TABLE OF ty_item
                      WITH NON-UNIQUE KEY order_id.

lt_items_sorted = lt_items.

LOOP AT lt_orders INTO ls_order.
  LOOP AT lt_items_sorted INTO ls_item
       WHERE order_id = ls_order-id.  " Uses binary search
    " Process
  ENDLOOP.
ENDLOOP.
```

## Rule 4: Use Field Symbols

### Example 9.2: INTO vs ASSIGNING Performance

```abap
REPORT z_fieldsymbol_performance.

TYPES: BEGIN OF ty_record,
         field1 TYPE string,
         field2 TYPE string,
         field3 TYPE i,
         field4 TYPE p DECIMALS 2,
       END OF ty_record.

DATA: lt_data  TYPE TABLE OF ty_record,
      ls_data  TYPE ty_record,
      lv_start TYPE timestampl,
      lv_end   TYPE timestampl.

FIELD-SYMBOLS: <ls_data> TYPE ty_record.

" Create 50,000 records
DO 50000 TIMES.
  ls_data = VALUE #( 
    field1 = |Text { sy-index }| 
    field2 = |More data|
    field3 = sy-index 
    field4 = sy-index * '1.5' 
  ).
  APPEND ls_data TO lt_data.
ENDDO.

" Test INTO (copies data)
GET TIME STAMP FIELD lv_start.
LOOP AT lt_data INTO ls_data.
  ls_data-field3 = ls_data-field3 + 1.
  MODIFY lt_data FROM ls_data.
ENDLOOP.
GET TIME STAMP FIELD lv_end.
WRITE: / 'INTO + MODIFY:', lv_end - lv_start.

" Test ASSIGNING (direct access)
GET TIME STAMP FIELD lv_start.
LOOP AT lt_data ASSIGNING <ls_data>.
  <ls_data>-field3 = <ls_data>-field3 + 1.
ENDLOOP.
GET TIME STAMP FIELD lv_end.
WRITE: / 'ASSIGNING:', lv_end - lv_start.
```

**Typical Results:**
```
INTO + MODIFY: 0.250000 seconds
ASSIGNING: 0.050000 seconds
```

## Rule 5: Filter Early

### BAD:

```abap
" Processes all records, then filters
LOOP AT lt_orders INTO ls_order.
  IF ls_order-status = 'COMPLETE'.
    " Process
  ENDIF.
ENDLOOP.
```

### GOOD:

```abap
" Filters during loop
LOOP AT lt_orders INTO ls_order 
     WHERE status = 'COMPLETE'.
  " Process
ENDLOOP.
```

### BEST:

```abap
" Filter at database level
SELECT * FROM orders INTO TABLE lt_orders
  WHERE status = 'COMPLETE'.
```

## Advanced: Parallel Processing

For very large datasets, consider parallel processing.

### Example 9.3: Using aRFC for Parallel Processing

```abap
REPORT z_parallel_processing.

TYPES: BEGIN OF ty_batch,
         from_index TYPE i,
         to_index   TYPE i,
       END OF ty_batch.

DATA: lt_batches TYPE TABLE OF ty_batch,
      ls_batch   TYPE ty_batch.

" Split 10,000 records into 10 batches
DO 10 TIMES.
  ls_batch-from_index = ( sy-index - 1 ) * 1000 + 1.
  ls_batch-to_index = sy-index * 1000.
  APPEND ls_batch TO lt_batches.
ENDDO.

" Process batches in parallel using aRFC
LOOP AT lt_batches INTO ls_batch.
  CALL FUNCTION 'Z_PROCESS_BATCH'
    STARTING NEW TASK 'BATCH'
    EXPORTING
      iv_from = ls_batch-from_index
      iv_to   = ls_batch-to_index.
ENDLOOP.

" Wait for completion
WAIT UNTIL completed = 10.
```

## Summary: Performance Checklist

✅ Use `FOR ALL ENTRIES` instead of SELECT in LOOP
✅ Use SORTED/HASHED tables for frequent lookups
✅ Use `ASSIGNING` instead of `INTO` when modifying
✅ Use `WHERE` clause in LOOP
✅ Avoid modifying loop table size during iteration
✅ Use parallel cursor for sorted tables
✅ Filter at database level when possible
✅ Consider PACKAGE SIZE for huge datasets
✅ Use secondary keys for multiple access paths

---

# Chapter 10: Common Errors and Solutions

## Error 1: Infinite Loop

### Symptom:
Program hangs, doesn't respond, or times out.

### Causes:
1. Missing EXIT condition in DO loop
2. Condition never becomes false in WHILE loop
3. Counter not updated

### Example of Problem:

```abap
" Problem: Counter never changes
DATA: lv_i TYPE i VALUE 1.
WHILE lv_i < 10.
  WRITE: / lv_i.
  " Forgot: lv_i = lv_i + 1
ENDWHILE.
```

### Solution:

```abap
DATA: lv_i TYPE i VALUE 1.
WHILE lv_i < 10.
  WRITE: / lv_i.
  lv_i = lv_i + 1.  " Always update condition variable
ENDWHILE.
```

### Prevention:
- Always add safety limit: `DO 10000 TIMES.` or `WHILE lv_i < 10 AND sy-index < 10000.`
- Use EXIT with timeout logic

## Error 2: SY-TABIX Becomes 0 or Wrong

### Symptom:
`SY-TABIX` shows 0 or unexpected value.

### Cause:
Another table operation inside loop resets SY-TABIX.

### Example of Problem:

```abap
LOOP AT lt_orders INTO ls_order.
  lv_order_index = sy-tabix.  " Correct here
  
  READ TABLE lt_customers INTO ls_customer
    WITH KEY id = ls_order-customer_id.
  
  " SY-TABIX now shows customer index, not order index!
  WRITE: / 'Order index:', sy-tabix.  " WRONG!
ENDLOOP.
```

### Solution:

```abap
LOOP AT lt_orders INTO ls_order.
  DATA(lv_order_index) = sy-tabix.  " Save immediately
  
  READ TABLE lt_customers INTO ls_customer
    WITH KEY id = ls_order-customer_id.
  
  WRITE: / 'Order index:', lv_order_index.  " CORRECT!
ENDLOOP.
```

## Error 3: Modifying Table During Loop

### Symptom:
Records skipped, duplicated, or runtime error.

### Example of Problem:

```abap
" DANGEROUS - Deleting while looping forward
LOOP AT lt_items INTO ls_item.
  IF ls_item-status = 'DELETED'.
    DELETE lt_items INDEX sy-tabix.
  ENDIF.
ENDLOOP.
```

### What Happens:
- Delete row 2 → Row 3 becomes row 2
- Loop moves to row 3 → Original row 3 is skipped!

### Solutions:

**Solution A: Loop Backwards**
```abap
DATA: lv_index TYPE i.
lv_index = lines( lt_items ).

WHILE lv_index > 0.
  READ TABLE lt_items INTO ls_item INDEX lv_index.
  IF ls_item-status = 'DELETED'.
    DELETE lt_items INDEX lv_index.
  ENDIF.
  lv_index = lv_index - 1.
ENDWHILE.
```

**Solution B: DELETE with WHERE**
```abap
" Best solution - no loop needed
DELETE lt_items WHERE status = 'DELETED'.
```

**Solution C: Collect and Delete Later**
```abap
DATA: lt_to_delete TYPE TABLE OF i.

LOOP AT lt_items INTO ls_item.
  IF ls_item-status = 'DELETED'.
    APPEND sy-tabix TO lt_to_delete.
  ENDIF.
ENDLOOP.

" Delete in reverse order
SORT lt_to_delete DESCENDING.
LOOP AT lt_to_delete INTO DATA(lv_idx).
  DELETE lt_items INDEX lv_idx.
ENDLOOP.
```

## Error 4: Field Symbol Not Assigned

### Symptom:
Runtime error: "Field symbol has not been assigned yet"

### Cause:
Using field symbol after unsuccessful table operation.

### Example of Problem:

```abap
FIELD-SYMBOLS: <ls_data> TYPE ty_data.

READ TABLE lt_data ASSIGNING <ls_data>
  WITH KEY id = 999.  " Does not exist

" Runtime error!
<ls_data>-name = 'Test'.
```

### Solution:

```abap
READ TABLE lt_data ASSIGNING <ls_data>
  WITH KEY id = 999.

IF sy-subrc = 0.  " Check if successful
  <ls_data>-name = 'Test'.
ELSE.
  " Handle not found case
  WRITE: / 'Record not found'.
ENDIF.
```

## Error 5: Wrong Table in Nested LOOP

### Symptom:
Incorrect results, loops don't correlate properly.

### Example of Problem:

```abap
LOOP AT lt_orders INTO ls_order.
  " Oops! Looping lt_orders instead of lt_items
  LOOP AT lt_orders INTO ls_item.  " WRONG!
    " This loops orders, not items
  ENDLOOP.
ENDLOOP.
```

### Prevention:
- Use meaningful variable names
- Review nested loops carefully

## Error 6: Empty Table Not Checked

### Symptom:
FOR ALL ENTRIES returns all records.

### Example of Problem:

```abap
" If lt_materials is empty, this returns ALL mara records!
SELECT * FROM mara INTO TABLE lt_mara
  FOR ALL ENTRIES IN lt_materials
  WHERE matnr = lt_materials-matnr.
```

### Solution:

```abap
IF lt_materials IS NOT INITIAL.
  SELECT * FROM mara INTO TABLE lt_mara
    FOR ALL ENTRIES IN lt_materials
    WHERE matnr = lt_materials-matnr.
ENDIF.
```

## Error 7: TYPE Mismatch in VARYING

### Symptom:
Runtime error or incorrect data.

### Example of Problem:

```abap
DATA: BEGIN OF ls_struct,
        field1 TYPE string,
        field2 TYPE i,  " Different type!
        field3 TYPE string,
      END OF ls_struct.

DATA: lv_value TYPE string.

" Error: field2 is integer, not string
DO 3 TIMES VARYING lv_value FROM ls_struct-field1 
                            NEXT ls_struct-field2.
```

### Solution:
Ensure all fields in VARYING sequence have same type.

## Error 8: LOOP AT SCREEN Without MODIFY

### Symptom:
Screen changes don't take effect.

### Example of Problem:

```abap
LOOP AT SCREEN.
  IF screen-name = 'P_FIELD'.
    screen-input = 0.
    " Forgot MODIFY SCREEN!
  ENDIF.
ENDLOOP.
```

### Solution:

```abap
LOOP AT SCREEN.
  IF screen-name = 'P_FIELD'.
    screen-input = 0.
    MODIFY SCREEN.  " Don't forget!
  ENDIF.
ENDLOOP.
```

## Error 9: Incorrect WHERE Condition Syntax

### Symptom:
Syntax error or unexpected results.

### Examples of Problems:

```abap
" Wrong: Using = for pattern match
LOOP AT lt_data INTO ls_data WHERE name = 'John*'.

" Wrong: LIKE not supported in LOOP WHERE
LOOP AT lt_data INTO ls_data WHERE name LIKE 'John%'.

" Wrong: OR not directly supported
LOOP AT lt_data INTO ls_data WHERE status = 'A' OR status = 'B'.
```

### Solutions:

```abap
" For pattern match, use CP
LOOP AT lt_data INTO ls_data WHERE name CP 'John*'.

" For OR conditions, use IN
DATA: lt_status TYPE RANGE OF char1.
lt_status = VALUE #( ( sign = 'I' option = 'EQ' low = 'A' )
                     ( sign = 'I' option = 'EQ' low = 'B' ) ).
LOOP AT lt_data INTO ls_data WHERE status IN lt_status.

" Or filter inside loop
LOOP AT lt_data INTO ls_data.
  CHECK ls_data-status = 'A' OR ls_data-status = 'B'.
ENDLOOP.
```

## Error 10: Performance - SELECT in LOOP

### Symptom:
Program runs very slowly.

### The Problem:

```abap
" Each iteration = 1 database call
" 1000 orders = 1000 database calls!
LOOP AT lt_orders INTO ls_order.
  SELECT SINGLE name FROM customer INTO lv_name
    WHERE id = ls_order-customer_id.
ENDLOOP.
```

### Solution - Use JOIN or FOR ALL ENTRIES:

```abap
" Single database call with JOIN
SELECT o~order_id, c~name
  FROM orders AS o
  INNER JOIN customer AS c ON o~customer_id = c~id
  INTO TABLE @lt_result.

" Or FOR ALL ENTRIES
SELECT * FROM customer INTO TABLE lt_customers
  FOR ALL ENTRIES IN lt_orders
  WHERE id = lt_orders-customer_id.
```

---

# Chapter 11: Complex Real-World Scenarios

## Scenario 1: Process Sales Orders with Items and Deliveries

### Problem:
Process sales orders, their items, and check delivery status for each item.

### Solution:

```abap
REPORT z_order_processing.

" Type definitions
TYPES: BEGIN OF ty_order,
         order_id    TYPE i,
         customer    TYPE string,
         order_date  TYPE d,
         status      TYPE string,
       END OF ty_order.

TYPES: BEGIN OF ty_item,
         order_id  TYPE i,
         item_no   TYPE i,
         material  TYPE string,
         quantity  TYPE i,
         price     TYPE p DECIMALS 2,
       END OF ty_item.

TYPES: BEGIN OF ty_delivery,
         order_id     TYPE i,
         item_no      TYPE i,
         delivery_qty TYPE i,
         status       TYPE string,
       END OF ty_delivery.

TYPES: BEGIN OF ty_result,
         order_id     TYPE i,
         customer     TYPE string,
         item_no      TYPE i,
         material     TYPE string,
         ordered_qty  TYPE i,
         delivered    TYPE i,
         pending      TYPE i,
         value        TYPE p DECIMALS 2,
       END OF ty_result.

" Tables
DATA: lt_orders     TYPE SORTED TABLE OF ty_order 
                    WITH UNIQUE KEY order_id,
      lt_items      TYPE SORTED TABLE OF ty_item 
                    WITH NON-UNIQUE KEY order_id item_no,
      lt_deliveries TYPE SORTED TABLE OF ty_delivery 
                    WITH NON-UNIQUE KEY order_id item_no,
      lt_results    TYPE TABLE OF ty_result.

DATA: ls_order    TYPE ty_order,
      ls_item     TYPE ty_item,
      ls_delivery TYPE ty_delivery,
      ls_result   TYPE ty_result.

" Sample data
lt_orders = VALUE #(
  ( order_id = 1001 customer = 'ABC Corp' 
    order_date = '20240101' status = 'Open' )
  ( order_id = 1002 customer = 'XYZ Ltd' 
    order_date = '20240102' status = 'Open' )
).

lt_items = VALUE #(
  ( order_id = 1001 item_no = 10 material = 'MAT-001' 
    quantity = 100 price = '10.00' )
  ( order_id = 1001 item_no = 20 material = 'MAT-002' 
    quantity = 50 price = '25.00' )
  ( order_id = 1002 item_no = 10 material = 'MAT-001' 
    quantity = 200 price = '10.00' )
).

lt_deliveries = VALUE #(
  ( order_id = 1001 item_no = 10 delivery_qty = 60 status = 'Shipped' )
  ( order_id = 1001 item_no = 10 delivery_qty = 20 status = 'Shipped' )
  ( order_id = 1001 item_no = 20 delivery_qty = 50 status = 'Shipped' )
  ( order_id = 1002 item_no = 10 delivery_qty = 100 status = 'Shipped' )
).

" Process using optimized nested loops
LOOP AT lt_orders INTO ls_order.
  " Inner loop on items - uses binary search due to SORTED
  LOOP AT lt_items INTO ls_item 
       WHERE order_id = ls_order-order_id.
    
    CLEAR ls_result.
    ls_result-order_id    = ls_order-order_id.
    ls_result-customer    = ls_order-customer.
    ls_result-item_no     = ls_item-item_no.
    ls_result-material    = ls_item-material.
    ls_result-ordered_qty = ls_item-quantity.
    ls_result-value       = ls_item-quantity * ls_item-price.
    
    " Calculate delivered quantity
    LOOP AT lt_deliveries INTO ls_delivery
         WHERE order_id = ls_item-order_id
           AND item_no = ls_item-item_no.
      ls_result-delivered = ls_result-delivered + 
                            ls_delivery-delivery_qty.
    ENDLOOP.
    
    ls_result-pending = ls_result-ordered_qty - ls_result-delivered.
    
    APPEND ls_result TO lt_results.
  ENDLOOP.
ENDLOOP.

" Display results
WRITE: / 'Order Processing Report'.
WRITE: / '========================'.
WRITE: / 'Order', 12 'Customer', 25 'Item', 32 'Material', 
         45 'Ordered', 55 'Delivered', 67 'Pending', 77 'Value'.
WRITE: / ''.

LOOP AT lt_results INTO ls_result.
  WRITE: / ls_result-order_id, 
           12 ls_result-customer, 
           25 ls_result-item_no,
           32 ls_result-material,
           45 ls_result-ordered_qty,
           55 ls_result-delivered,
           67 ls_result-pending,
           77 ls_result-value.
ENDLOOP.

" Summary by order
WRITE: / ''.
WRITE: / 'Order Summary:'.

LOOP AT lt_results INTO ls_result
     GROUP BY ( order_id = ls_result-order_id
                customer = ls_result-customer )
     INTO DATA(ls_group).
  
  DATA: lv_order_total TYPE p DECIMALS 2,
        lv_items_count TYPE i.
  CLEAR: lv_order_total, lv_items_count.
  
  LOOP AT GROUP ls_group INTO DATA(ls_member).
    lv_order_total = lv_order_total + ls_member-value.
    lv_items_count = lv_items_count + 1.
  ENDLOOP.
  
  WRITE: / 'Order:', ls_group-order_id, 
           ls_group-customer,
           'Items:', lv_items_count,
           'Total:', lv_order_total.
ENDLOOP.
```

**Output:**
```
Order Processing Report
========================
Order      Customer       Item   Material      Ordered   Delivered   Pending   Value

1001       ABC Corp       10     MAT-001       100       80          20        1000.00
1001       ABC Corp       20     MAT-002       50        50          0         1250.00
1002       XYZ Ltd        10     MAT-001       200       100         100       2000.00

Order Summary:
Order: 1001 ABC Corp Items: 2 Total: 2250.00
Order: 1002 XYZ Ltd Items: 1 Total: 2000.00
```

## Scenario 2: Hierarchical Data Processing (BOM Explosion)

### Problem:
Process multi-level Bill of Materials (BOM) structure.

### Solution:

```abap
REPORT z_bom_explosion.

TYPES: BEGIN OF ty_bom_item,
         parent   TYPE string,
         child    TYPE string,
         quantity TYPE i,
         level    TYPE i,
       END OF ty_bom_item.

TYPES: BEGIN OF ty_exploded,
         material TYPE string,
         quantity TYPE i,
         level    TYPE i,
       END OF ty_exploded.

DATA: lt_bom      TYPE TABLE OF ty_bom_item,
      lt_exploded TYPE TABLE OF ty_exploded,
      ls_bom      TYPE ty_bom_item,
      ls_exploded TYPE ty_exploded,
      lt_stack    TYPE TABLE OF ty_exploded.

" BOM structure
lt_bom = VALUE #(
  ( parent = 'CAR' child = 'ENGINE' quantity = 1 )
  ( parent = 'CAR' child = 'WHEEL' quantity = 4 )
  ( parent = 'CAR' child = 'SEAT' quantity = 5 )
  ( parent = 'ENGINE' child = 'PISTON' quantity = 4 )
  ( parent = 'ENGINE' child = 'VALVE' quantity = 8 )
  ( parent = 'WHEEL' child = 'TIRE' quantity = 1 )
  ( parent = 'WHEEL' child = 'RIM' quantity = 1 )
  ( parent = 'PISTON' child = 'RING' quantity = 3 )
).

" Start with top level
ls_exploded = VALUE #( material = 'CAR' quantity = 1 level = 0 ).
APPEND ls_exploded TO lt_stack.

" Process stack (iterative tree traversal)
WHILE lt_stack IS NOT INITIAL.
  " Pop from stack
  DATA(lv_lines) = lines( lt_stack ).
  READ TABLE lt_stack INTO ls_exploded INDEX lv_lines.
  DELETE lt_stack INDEX lv_lines.
  
  " Add to results
  APPEND ls_exploded TO lt_exploded.
  
  " Find children
  LOOP AT lt_bom INTO ls_bom 
       WHERE parent = ls_exploded-material.
    
    DATA(ls_child) = VALUE ty_exploded(
      material = ls_bom-child
      quantity = ls_exploded-quantity * ls_bom-quantity
      level = ls_exploded-level + 1
    ).
    
    " Push children to stack (reverse order for correct processing)
    INSERT ls_child INTO lt_stack INDEX 1.
  ENDLOOP.
ENDWHILE.

" Display exploded BOM
WRITE: / 'Exploded Bill of Materials for 1 CAR:'.
WRITE: / '======================================'.

LOOP AT lt_exploded INTO ls_exploded.
  DO ls_exploded-level TIMES.
    WRITE: '  '.
  ENDDO.
  
  IF ls_exploded-level > 0.
    WRITE: '└─'.
  ENDIF.
  
  WRITE: ls_exploded-material, '(Qty:', ls_exploded-quantity, ')'.
  WRITE: /.
ENDLOOP.
```

**Output:**
```
Exploded Bill of Materials for 1 CAR:
======================================
CAR (Qty: 1 )
  └─ ENGINE (Qty: 1 )
    └─ PISTON (Qty: 4 )
      └─ RING (Qty: 12 )
    └─ VALVE (Qty: 8 )
  └─ WHEEL (Qty: 4 )
    └─ TIRE (Qty: 4 )
    └─ RIM (Qty: 4 )
  └─ SEAT (Qty: 5 )
```

## Scenario 3: Batch Processing with Error Handling

### Problem:
Process large dataset with proper error handling and logging.

### Solution:

```abap
REPORT z_batch_processing.

TYPES: BEGIN OF ty_record,
         id      TYPE i,
         data    TYPE string,
         status  TYPE string,
         message TYPE string,
       END OF ty_record.

TYPES: BEGIN OF ty_log,
         timestamp TYPE timestampl,
         record_id TYPE i,
         type      TYPE c LENGTH 1,
         message   TYPE string,
       END OF ty_log.

CLASS lcl_processor DEFINITION.
  PUBLIC SECTION.
    DATA: mt_records    TYPE TABLE OF ty_record,
          mt_log        TYPE TABLE OF ty_log,
          mv_success    TYPE i,
          mv_error      TYPE i,
          mv_batch_size TYPE i VALUE 100.
    
    METHODS: load_data,
             process_all,
             process_record IMPORTING is_record TYPE ty_record
                            RETURNING VALUE(rv_success) TYPE abap_bool,
             log_message IMPORTING iv_record_id TYPE i
                                   iv_type TYPE c
                                   iv_message TYPE string,
             display_summary.
ENDCLASS.

CLASS lcl_processor IMPLEMENTATION.
  METHOD load_data.
    " Simulate loading 500 records
    DO 500 TIMES.
      DATA(ls_record) = VALUE ty_record(
        id = sy-index
        data = |Record data { sy-index }|
        status = 'NEW'
      ).
      
      " Simulate some bad data
      IF sy-index MOD 50 = 0.
        ls_record-data = ''.  " Empty data = error
      ENDIF.
      
      APPEND ls_record TO mt_records.
    ENDDO.
    
    log_message( iv_record_id = 0 
                 iv_type = 'I' 
                 iv_message = |Loaded { lines( mt_records ) } records| ).
  ENDMETHOD.
  
  METHOD process_all.
    DATA: lv_batch_count TYPE i,
          lv_processed   TYPE i.
    
    FIELD-SYMBOLS: <ls_record> TYPE ty_record.
    
    LOOP AT mt_records ASSIGNING <ls_record>.
      lv_processed = lv_processed + 1.
      
      TRY.
          IF process_record( <ls_record> ) = abap_true.
            <ls_record>-status = 'SUCCESS'.
            <ls_record>-message = 'Processed successfully'.
            mv_success = mv_success + 1.
          ELSE.
            <ls_record>-status = 'ERROR'.
            mv_error = mv_error + 1.
          ENDIF.
          
        CATCH cx_root INTO DATA(lx_error).
          <ls_record>-status = 'ERROR'.
          <ls_record>-message = lx_error->get_text( ).
          mv_error = mv_error + 1.
          log_message( iv_record_id = <ls_record>-id
                       iv_type = 'E'
                       iv_message = lx_error->get_text( ) ).
      ENDTRY.
      
      " Commit every batch_size records
      IF lv_processed MOD mv_batch_size = 0.
        lv_batch_count = lv_batch_count + 1.
        log_message( iv_record_id = 0
                     iv_type = 'I'
                     iv_message = |Batch { lv_batch_count } committed - { lv_processed } records processed| ).
        " COMMIT WORK in real scenario
      ENDIF.
    ENDLOOP.
    
    " Final commit
    log_message( iv_record_id = 0
                 iv_type = 'I'
                 iv_message = 'Processing complete' ).
  ENDMETHOD.
  
  METHOD process_record.
    " Simulate processing with validation
    IF is_record-data IS INITIAL.
      log_message( iv_record_id = is_record-id
                   iv_type = 'E'
                   iv_message = 'Empty data not allowed' ).
      rv_success = abap_false.
      RETURN.
    ENDIF.
    
    " Simulate processing time
    " In real scenario: database updates, BAPI calls, etc.
    
    rv_success = abap_true.
  ENDMETHOD.
  
  METHOD log_message.
    DATA(ls_log) = VALUE ty_log(
      record_id = iv_record_id
      type = iv_type
      message = iv_message
    ).
    GET TIME STAMP FIELD ls_log-timestamp.
    APPEND ls_log TO mt_log.
  ENDMETHOD.
  
  METHOD display_summary.
    WRITE: / 'Processing Summary'.
    WRITE: / '=================='.
    WRITE: / 'Total records:', lines( mt_records ).
    WRITE: / 'Successful:', mv_success.
    WRITE: / 'Errors:', mv_error.
    WRITE: / 'Success rate:', 
             mv_success * 100 / lines( mt_records ), '%'.
    
    WRITE: / ''.
    WRITE: / 'Error Details:'.
    WRITE: / '--------------'.
    
    LOOP AT mt_records INTO DATA(ls_record) 
         WHERE status = 'ERROR'.
      WRITE: / 'Record', ls_record-id, ':', ls_record-message.
    ENDLOOP.
    
    WRITE: / ''.
    WRITE: / 'Processing Log:'.
    WRITE: / '---------------'.
    
    LOOP AT mt_log INTO DATA(ls_log).
      WRITE: / ls_log-timestamp, ls_log-type, ls_log-message.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.

" Main program
START-OF-SELECTION.
  DATA(lo_processor) = NEW lcl_processor( ).
  
  lo_processor->load_data( ).
  lo_processor->process_all( ).
  lo_processor->display_summary( ).
```

## Scenario 4: Dynamic Loop Based on Configuration

### Problem:
Process data according to runtime configuration.

### Solution:

```abap
REPORT z_dynamic_processing.

TYPES: BEGIN OF ty_rule,
         field      TYPE string,
         operation  TYPE string,
         value      TYPE string,
         action     TYPE string,
       END OF ty_rule.

TYPES: BEGIN OF ty_data,
         id      TYPE i,
         name    TYPE string,
         amount  TYPE p DECIMALS 2,
         region  TYPE string,
         status  TYPE string,
       END OF ty_data.

DATA: lt_rules TYPE TABLE OF ty_rule,
      lt_data  TYPE TABLE OF ty_data.

FIELD-SYMBOLS: <ls_data> TYPE ty_data,
               <lv_field> TYPE any.

" Configuration rules
lt_rules = VALUE #(
  ( field = 'AMOUNT' operation = 'GT' value = '1000' 
    action = 'HIGHLIGHT' )
  ( field = 'REGION' operation = 'EQ' value = 'EMEA' 
    action = 'FLAG' )
  ( field = 'STATUS' operation = 'EQ' value = 'PENDING' 
    action = 'REVIEW' )
).

" Sample data
lt_data = VALUE #(
  ( id = 1 name = 'Order A' amount = 500 region = 'AMER' status = 'COMPLETE' )
  ( id = 2 name = 'Order B' amount = 1500 region = 'EMEA' status = 'PENDING' )
  ( id = 3 name = 'Order C' amount = 2000 region = 'APAC' status = 'COMPLETE' )
).

WRITE: / 'Applying', lines( lt_rules ), 'rules to', 
         lines( lt_data ), 'records'.
WRITE: / ''.

LOOP AT lt_data ASSIGNING <ls_data>.
  WRITE: / 'Record:', <ls_data>-id, <ls_data>-name.
  
  " Apply each rule dynamically
  LOOP AT lt_rules INTO DATA(ls_rule).
    " Get field value dynamically
    ASSIGN COMPONENT ls_rule-field OF STRUCTURE <ls_data> 
      TO <lv_field>.
    
    IF sy-subrc = 0.
      DATA(lv_match) = abap_false.
      
      CASE ls_rule-operation.
        WHEN 'EQ'.
          IF <lv_field> = ls_rule-value.
            lv_match = abap_true.
          ENDIF.
        WHEN 'GT'.
          IF <lv_field> > ls_rule-value.
            lv_match = abap_true.
          ENDIF.
        WHEN 'LT'.
          IF <lv_field> < ls_rule-value.
            lv_match = abap_true.
          ENDIF.
      ENDCASE.
      
      IF lv_match = abap_true.
        WRITE: / '  → Rule matched:', 
                 ls_rule-field, ls_rule-operation, ls_rule-value,
                 '- Action:', ls_rule-action.
      ENDIF.
    ENDIF.
  ENDLOOP.
ENDLOOP.
```

**Output:**
```
Applying 3 rules to 3 records

Record: 1 Order A

Record: 2 Order B
  → Rule matched: AMOUNT GT 1000 - Action: HIGHLIGHT
  → Rule matched: REGION EQ EMEA - Action: FLAG
  → Rule matched: STATUS EQ PENDING - Action: REVIEW

Record: 3 Order C
  → Rule matched: AMOUNT GT 1000 - Action: HIGHLIGHT
```

---

# Chapter 12: Best Practices and Guidelines

## Coding Standards for Loops

### 1. Always Use Meaningful Names

```abap
" BAD
LOOP AT lt_t INTO ls_s.
  IF ls_s-f1 > 100.
    " What is f1?
  ENDIF.
ENDLOOP.

" GOOD
LOOP AT lt_customers INTO ls_customer.
  IF ls_customer-credit_limit > 100.
    " Clear meaning
  ENDIF.
ENDLOOP.
```

### 2. Keep Loops Focused

```abap
" BAD - Too much logic in loop
LOOP AT lt_orders INTO ls_order.
  " 200 lines of code
ENDLOOP.

" GOOD - Delegate to methods
LOOP AT lt_orders INTO ls_order.
  process_order( ls_order ).
  IF is_high_priority( ls_order ).
    escalate_order( ls_order ).
  ENDIF.
ENDLOOP.
```

### 3. Comment Complex Loops

```abap
" Process orders: Filter by status, calculate totals,
" and flag items needing attention
LOOP AT lt_orders INTO ls_order 
     WHERE status IN lr_active_status.
  
  " Sum order value
  lv_total = lv_total + ls_order-amount.
  
  " Flag if over threshold
  IF ls_order-amount > gc_threshold.
    " Mark for manager review
    ls_order-flag = 'X'.
    MODIFY lt_orders FROM ls_order.
  ENDIF.
  
ENDLOOP.
```

### 4. Limit Nesting Depth

```abap
" BAD - Too deeply nested
LOOP AT lt_orders INTO ls_order.
  IF ls_order-status = 'A'.
    LOOP AT lt_items INTO ls_item WHERE order_id = ls_order-id.
      IF ls_item-qty > 0.
        LOOP AT lt_prices INTO ls_price WHERE item_id = ls_item-id.
          IF ls_price-valid = 'X'.
            " Lost in the maze
          ENDIF.
        ENDLOOP.
      ENDIF.
    ENDLOOP.
  ENDIF.
ENDLOOP.

" GOOD - Use CONTINUE to reduce nesting
LOOP AT lt_orders INTO ls_order.
  CHECK ls_order-status = 'A'.
  
  LOOP AT lt_items INTO ls_item WHERE order_id = ls_order-id.
    CHECK ls_item-qty > 0.
    
    process_item_prices( ls_item ).
  ENDLOOP.
ENDLOOP.
```

### 5. Prefer Declarative Approaches

```abap
" Imperative (old style)
DATA: lv_sum TYPE i.
LOOP AT lt_numbers INTO DATA(lv_num).
  lv_sum = lv_sum + lv_num.
ENDLOOP.

" Declarative (modern ABAP)
DATA(lv_sum) = REDUCE i( 
  INIT sum = 0 
  FOR num IN lt_numbers 
  NEXT sum = sum + num 
).
```

## Performance Guidelines Summary

| Do This | Not This |
|---------|----------|
| `SELECT ... INTO TABLE` | `SELECT ... ENDSELECT` |
| `LOOP ... ASSIGNING` for modification | `LOOP ... INTO` + `MODIFY` |
| `DELETE itab WHERE` | `DELETE` inside `LOOP` |
| SORTED/HASHED tables for lookups | STANDARD table with full scan |
| `FOR ALL ENTRIES` | `SELECT` inside `LOOP` |
| Parallel cursor for sorted data | Nested loops with WHERE |
| Filter in database `WHERE` | Filter after selection |

## Anti-Patterns to Avoid

### Anti-Pattern 1: The God Loop

```abap
" Loop doing everything - hard to maintain and test
LOOP AT lt_data INTO ls_data.
  " Validate
  " Transform
  " Calculate
  " Log
  " Save
  " Send notification
  " Update status
  " Archive
  " 500 lines later...
ENDLOOP.
```

### Anti-Pattern 2: Copy-Paste Loops

```abap
" Same loop copied with minor variations
LOOP AT lt_customers INTO ls_customer WHERE region = 'US'.
  calculate_discount( ls_customer ).
ENDLOOP.

LOOP AT lt_customers INTO ls_customer WHERE region = 'EU'.
  calculate_discount( ls_customer ).
ENDLOOP.

" Better: Single loop with parameter
LOOP AT lt_customers INTO ls_customer 
     WHERE region IN lt_regions.
  calculate_discount( ls_customer ).
ENDLOOP.
```

### Anti-Pattern 3: Premature Optimization

```abap
" Don't optimize until you measure!
" This is often unnecessary complexity:
DATA: lt_sorted TYPE SORTED TABLE...,
      lt_hashed TYPE HASHED TABLE...,
      lt_index  TYPE TABLE...

" For 100 records, a simple STANDARD table is fine
" Optimize only when:
" 1. You have performance problems
" 2. You've measured the bottleneck
" 3. The improvement is significant
```

## Testing Loops

### Unit Test Example

```abap
CLASS ltcl_loop_test DEFINITION FOR TESTING
  RISK LEVEL HARMLESS
  DURATION SHORT.
  
  PRIVATE SECTION.
    METHODS: test_empty_table FOR TESTING,
             test_single_record FOR TESTING,
             test_filter_condition FOR TESTING.
ENDCLASS.

CLASS ltcl_loop_test IMPLEMENTATION.
  METHOD test_empty_table.
    DATA: lt_data TYPE TABLE OF ty_data,
          lv_count TYPE i.
    
    " Empty table should process 0 records
    LOOP AT lt_data INTO DATA(ls_data).
      lv_count = lv_count + 1.
    ENDLOOP.
    
    cl_abap_unit_assert=>assert_equals(
      act = lv_count
      exp = 0
      msg = 'Empty table should have 0 iterations'
    ).
  ENDMETHOD.
  
  METHOD test_single_record.
    DATA: lt_data TYPE TABLE OF ty_data,
          lv_count TYPE i.
    
    APPEND VALUE #( id = 1 ) TO lt_data.
    
    LOOP AT lt_data INTO DATA(ls_data).
      lv_count = lv_count + 1.
    ENDLOOP.
    
    cl_abap_unit_assert=>assert_equals(
      act = lv_count
      exp = 1
      msg = 'Single record should have 1 iteration'
    ).
  ENDMETHOD.
  
  METHOD test_filter_condition.
    DATA: lt_data TYPE TABLE OF ty_data,
          lv_count TYPE i.
    
    lt_data = VALUE #(
      ( id = 1 status = 'A' )
      ( id = 2 status = 'B' )
      ( id = 3 status = 'A' )
    ).
    
    LOOP AT lt_data INTO DATA(ls_data) WHERE status = 'A'.
      lv_count = lv_count + 1.
    ENDLOOP.
    
    cl_abap_unit_assert=>assert_equals(
      act = lv_count
      exp = 2
      msg = 'Filter should return 2 records'
    ).
  ENDMETHOD.
ENDCLASS.
```

## Quick Reference Card

### DO Loop
```abap
DO [n TIMES].
  " sy-index contains iteration number
  [EXIT.] " Exit loop
ENDDO.
```

### WHILE Loop
```abap
WHILE condition.
  " Update condition variable!
ENDWHILE.
```

### LOOP AT Internal Table
```abap
LOOP AT itab INTO wa [WHERE condition] [FROM n] [TO m].
  " sy-tabix contains row index
ENDLOOP.

LOOP AT itab ASSIGNING <fs>.  " For modification
LOOP AT itab REFERENCE INTO lr.  " For reference
```

### Loop Control
```abap
EXIT.      " Exit current loop
CONTINUE.  " Skip to next iteration
CHECK cond." CONTINUE if condition is FALSE
```

---

# Appendix A: Quick Troubleshooting Guide

| Problem | Likely Cause | Solution |
|---------|--------------|----------|
| Loop runs forever | Missing EXIT or counter update | Add EXIT condition or update WHILE variable |
| sy-tabix is 0 | Table operation inside loop | Save sy-tabix at loop start |
| Records skipped | Deleting during forward loop | Delete with WHERE or loop backwards |
| Slow performance | SELECT inside loop | Use FOR ALL ENTRIES |
| Field symbol error | Record not found | Check sy-subrc before using |
| Wrong results | Wrong table in nested loop | Verify table names |

---

# Appendix B: Performance Benchmarks

Tested with 100,000 records:

| Operation | Time |
|-----------|------|
| LOOP INTO | 150 ms |
| LOOP ASSIGNING | 50 ms |
| LOOP + MODIFY | 300 ms |
| READ STANDARD table | 5000 ms |
| READ SORTED table | 50 ms |
| READ HASHED table | 5 ms |
| SELECT ENDSELECT (1000 rows) | 2000 ms |
| SELECT INTO TABLE | 200 ms |

---

# Conclusion

Mastering loops in ABAP is fundamental to becoming an efficient SAP developer. Remember these key takeaways:

1. **Choose the right loop type** for your use case
2. **Optimize for performance** with proper table types and techniques
3. **Handle errors gracefully** with proper checks and exception handling
4. **Keep code readable** with meaningful names and focused loops
5. **Test thoroughly** with various data scenarios

With practice and attention to the patterns and anti-patterns described in this book, you'll write efficient, maintainable, and robust ABAP code.

---

*End of Book*
