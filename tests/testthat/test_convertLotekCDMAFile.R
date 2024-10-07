skip_on_cran()

dummy_file <- tempfile()
sink(dummy_file)
cat(
"WHS FSK Receiver Data File

Receiver Configuration: 
Working Frequency:  76 KHz
Bit Rate:           2400 bps
Code Type:          FSK
Serial Number:      WHS3K-1234567
Node ID:            10000

Decoded Tag Data:
Date      Time             TOA       Tag ID    Type     Value     Power
=======================================================================
04/09/24  22:50:03     0.43875        37910       P       9.1        12
08/21/24  12:45:18     0.99646        55606       M         0         1
08/23/24  15:01:04     0.76042        55778       P       0.0         2

Receiver Sensor Messages:
Date      Time      Sensor   Temp     Press   Battery  Tilt-X  Tilt-Y  Tilt-Z
=============================================================================
04/11/24  21:44:00  T / P    1534         0                                  

Receiver Setup Messages:
Date      Time      Type                    Details                                                     
=============================================================================
08/22/24  18:50:11  Change Logging Mode     New Mode: SETUP
")
sink()

test_that("convertLotekCDMAfile can read lotek cdma log files", {
	x <- convertLotekCDMAFile(dummy_file, date_format = "%m/%d/%y", 
                 	          tz = "Europe/Copenhagen")
	expect_is(x, "data.table")
})


dummy_file2 <- tempfile()
sink(dummy_file2)
cat(
"WHS FSK Receiver Data File

Receiver Configuration: 
Working Frequency:  76 KHz
Bit Rate:           2400 bps
Code Type:          FSK
Serial Number:      WHS3K-1234567
Node ID:            10000

Decoded Tag Data:
Date      Time             TOA       Tag ID    Type     Value     Power
=======================================================================
04/09/24  22:50:03     0.43875        37910       P       9.1        12
13/21/24  12:45:18     0.99646        55606       M         0         1
08/23/24  15:01:04     0.76042        55778       P       0.0         2

Receiver Sensor Messages:
Date      Time      Sensor   Temp     Press   Battery  Tilt-X  Tilt-Y  Tilt-Z
=============================================================================
04/11/24  21:44:00  T / P    1534         0                                  

Receiver Setup Messages:
Date      Time      Type                    Details                                                     
=============================================================================
08/22/24  18:50:11  Change Logging Mode     New Mode: SETUP
")
sink()

test_that("convertLotekCDMAfile warns user if any timestamp is bad", {
	expect_warning(
		x <- convertLotekCDMAFile(dummy_file2, date_format = "%m/%d/%y", 
        		                  tz = "Europe/Copenhagen"),
        paste0("Some timestamp values are NA. This must be fixed before these ",
            "detections are used in an actel analysis."),
      fixed = TRUE)
    expect_is(x, "data.table")
    expect_true(sum(is.na(x)) == 1)
})



dummy_file3 <- tempfile()
sink(dummy_file3)
cat(
"WHS FSK Receiver Data File

Receiver Configuration: 
Working Frequency:  76 KHz
Bit Rate:           2400 bps
Code Type:          FSK
Serial Number:      WHS3K-12b67
Node ID:            10000

Decoded Tag Data:
Date      Time             TOA       Tag ID    Type     Value     Power
=======================================================================
04/09/24  22:50:03     0.43875        37910       P       9.1        12
08/21/24  12:45:18     0.99646        55606       M         0         1
08/23/24  15:01:04     0.76042        55778       P       0.0         2

Receiver Sensor Messages:
Date      Time      Sensor   Temp     Press   Battery  Tilt-X  Tilt-Y  Tilt-Z
=============================================================================
04/11/24  21:44:00  T / P    1534         0                                  

Receiver Setup Messages:
Date      Time      Type                    Details                                                     
=============================================================================
08/22/24  18:50:11  Change Logging Mode     New Mode: SETUP
")
sink()

test_that("convertLotekCDMAfile warns user if any receiver serial is bad", {
	expect_warning(
		x <- convertLotekCDMAFile(dummy_file3, date_format = "%m/%d/%y", 
        		                  tz = "Europe/Copenhagen"),
        paste0("Some receiver serial number values are NA. This must be fixed ",
            "before these detections are used in an actel analysis."),
      fixed = TRUE)
    expect_is(x, "data.table")
    expect_true(sum(is.na(x)) == 3)
})


dummy_file4 <- tempfile()
sink(dummy_file4)
cat(
"WHS FSK Receiver Data File

Receiver Configuration: 
Working Frequency:  76 KHz
Bit Rate:           2400 bps
Code Type:           
Serial Number:      WHS3K-1234567
Node ID:            10000

Decoded Tag Data:
Date      Time             TOA       Tag ID    Type     Value     Power
=======================================================================
04/09/24  22:50:03     0.43875        37910       P       9.1        12
08/21/24  12:45:18     0.99646        55606       M         0         1
08/23/24  15:01:04     0.76042        55778       P       0.0         2

Receiver Sensor Messages:
Date      Time      Sensor   Temp     Press   Battery  Tilt-X  Tilt-Y  Tilt-Z
=============================================================================
04/11/24  21:44:00  T / P    1534         0                                  

Receiver Setup Messages:
Date      Time      Type                    Details                                                     
=============================================================================
08/22/24  18:50:11  Change Logging Mode     New Mode: SETUP
")
sink()

test_that("convertLotekCDMAfile warns user if code space is bad", {
    expect_warning(
        x <- convertLotekCDMAFile(dummy_file4, date_format = "%m/%d/%y", 
                                  tz = "Europe/Copenhagen"),
        paste0("Some code space values are NA. This must be fixed ",
            "before these detections are used in an actel analysis."),
      fixed = TRUE)
    expect_is(x, "data.table")
    expect_true(sum(is.na(x)) == 3)
})

dummy_file5 <- tempfile()
sink(dummy_file5)
cat(
"WHS FSK Receiver Data File

Receiver Configuration: 
Working Frequency:  76 KHz
Bit Rate:           2400 bps
Code Type:          FSK
Serial Number:      WHS3K-1234567
Node ID:            10000

Decoded Tag Data:
Date      Time             TOA       Tag ID    Type     Value     Power
=======================================================================
04/09/24  22:50:03     0.43875        37a10       P       9.1        12
08/21/24  12:45:18     0.99646        55606       M         0         1
08/23/24  15:01:04     0.76042        55778       P       0.0         2

Receiver Sensor Messages:
Date      Time      Sensor   Temp     Press   Battery  Tilt-X  Tilt-Y  Tilt-Z
=============================================================================
04/11/24  21:44:00  T / P    1534         0                                  

Receiver Setup Messages:
Date      Time      Type                    Details                                                     
=============================================================================
08/22/24  18:50:11  Change Logging Mode     New Mode: SETUP
")
sink()

test_that("convertLotekCDMAfile warns user if any signal is bad", {
    expect_warning(
        x <- convertLotekCDMAFile(dummy_file5, date_format = "%m/%d/%y", 
                                  tz = "Europe/Copenhagen"),
        paste0("Some signal values are NA. This must be fixed before these ",
            "detections are used in an actel analysis."),
      fixed = TRUE)
    expect_is(x, "data.table")
    expect_true(sum(is.na(x)) == 1)
})

rm(list = ls())
