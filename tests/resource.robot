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
