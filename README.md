# STAT440 Project 2 

Column information:

each row of the training data indicates the results of an internet speed test conducted at a location in Canada

V01 - avg_d_kbps: average download speed recorded

V02 - avg_u_kbps: average upload speed recorded

V03 - avg_lat_ms - average latency in (ms)

V04 - test - number of test performed at the same time and location, that are then averaged together

V05 - devices - this is the type of device that was tested (mobile, pc, laptop)

V06 - year - the year in which the test was performed

V07/V08/V09 - PRUID (Province 2 digit id), CDUID (Census division id), DAUID (Dissemination area id) - these are IDs related to the date and location

V10 - SACTYPE - This is an indicator of how rural the area tested is.

V11/V12/V13/V14 - DA_POP, PCUID, PCTYPE, PCCLASS - Further IDs related to how rural the area tested is.


# Methods
V1 - v5, V11: MICE
V6, V7, V10: Check if previous and next value is the same, if not, refer to V8 or V9.
V8, V9, V12, V13, V14: replace with the nearest filled value.
