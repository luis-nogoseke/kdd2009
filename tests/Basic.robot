*** Settings ***
Documentation     A test suite with basic tests.
Library           Selenium2Library  15
Library           OperatingSystem
Resource          resource.robot
Suite Setup       Suite Setup
Suite Teardown    Suite Teardown
Test Setup        Test Setup
Test Teardown     Test Teardown

*** Test Cases ***
Simple Test
    [Documentation]  This test just uploads a test file and downloads produced
    ...  results.
    Choose File
    ...  id=file1
    ...  file_path=/home/nogoseke/tcc-kdd2009/tcc-data/orange_small_train.data
    Sleep  2
    Download Results  results.csv

Model determinism
    [Documentation]  This test does the prediction twice and compare the results
    ...  to see if they are equal.
    Choose File
    ...  id=file1
    ...  file_path=/home/nogoseke/tcc-kdd2009/tcc-data/orange_small_train.data
    Sleep  2
    Download Results  results1.csv
    Choose File
    ...  id=file1
    ...  file_path=/home/nogoseke/tcc-kdd2009/tcc-data/orange_small_train.data
    Sleep  2
    Download Results  results2.csv
    Compare files  results1.csv  results2.csv
