*** Settings ***
Library  lib/Basic.py

*** Keywords ***

Suite Setup
    Run Web App

Suite Teardown
    Stop Web App

Test Setup
    Open Browser
    ...  url=http://localhost:50000/
    ...  browser=googlechrome

Test Teardown
    Close Browser

Download Results
    [Arguments]  ${filename}
    ${url}=  Get Element Attribute  downloadData@href
    Download File  ${url}  ${filename}

Compare files
    [Arguments]  ${file1}  ${file2}
    ${diff}=  Run and Return RC  diff ${file1} ${file2}
    Should be Equal  ${diff}  ${0}
