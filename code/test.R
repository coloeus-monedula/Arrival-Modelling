example_df <-  data.frame(gridref = 'TL1234', stringsAsFactors = FALSE)
example_output <- gridref_to_coordinates(df = example_df, invar_gridref = 'gridref')


temp1 <- data.frame(other1 = 'A', other2 = 'B', 
                    square = c('TL1', 'TL12', 'TL12A', 'TL1234', 'TL123456', 'TL1234567', 'GB1234','TL12O', 'IN1234', 'WV2575'), 
                    stringsAsFactors = FALSE)
temp1a<-gridref_to_coordinates(temp1,'square')
