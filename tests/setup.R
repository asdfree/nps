if ( .Platform$OS.type == 'windows' ) memory.limit( 256000 )

library(lodown)
lodown( "nps" , output_dir = file.path( getwd() ) )
library(lodown)
# examine all available NPS microdata files
nps_cat <-
	get_catalog( "nps" ,
		output_dir = file.path( getwd() ) )

# 2015 only 
nps_cat <- subset( nps_cat , year == 2015 )
# download the microdata to your local computer
lodown( "nps" , nps_cat )

library(survey)

nps_df <- 
	readRDS( 
		file.path( getwd() , 
			"2015 transportation.rds" ) )

nps_design <- 
	svrepdesign( 
		data = nps_df , 
		repweights = "pstotwgt[0-9]" , 
		weights = ~ pstotwgt , 
		type = "Fay" , 
		rho = 0.29986 , 
		mse = TRUE
	)
nps_design <- 
	update( 
		nps_design , 
		
		age_category =
			factor( agec , levels = 2:5 , labels =
			c( "60-64" , "65-74" , "75-84" , "85+" ) ) ,
		
		gender = factor( gender , labels = c( "male" , "female" ) ) ,
		
		trip_this_week = as.numeric( trdays %in% 1:2 )

	)
sum( weights( nps_design , "sampling" ) != 0 )

svyby( ~ one , ~ age_category , nps_design , unwtd.count )
svytotal( ~ one , nps_design )

svyby( ~ one , ~ age_category , nps_design , svytotal )
svymean( ~ adlaoa6p , nps_design , na.rm = TRUE )

svyby( ~ adlaoa6p , ~ age_category , nps_design , svymean , na.rm = TRUE )
svymean( ~ gender , nps_design )

svyby( ~ gender , ~ age_category , nps_design , svymean )
svytotal( ~ adlaoa6p , nps_design , na.rm = TRUE )

svyby( ~ adlaoa6p , ~ age_category , nps_design , svytotal , na.rm = TRUE )
svytotal( ~ gender , nps_design )

svyby( ~ gender , ~ age_category , nps_design , svytotal )
svyquantile( ~ adlaoa6p , nps_design , 0.5 , na.rm = TRUE )

svyby( 
	~ adlaoa6p , 
	~ age_category , 
	nps_design , 
	svyquantile , 
	0.5 ,
	ci = TRUE ,
	keep.var = TRUE ,
	na.rm = TRUE
)
svyratio( 
	numerator = ~ adlaoa6p , 
	denominator = ~ iadlaoa7 , 
	nps_design ,
	na.rm = TRUE
)
sub_nps_design <- subset( nps_design , livealone == 1 )
svymean( ~ adlaoa6p , sub_nps_design , na.rm = TRUE )
this_result <- svymean( ~ adlaoa6p , nps_design , na.rm = TRUE )

coef( this_result )
SE( this_result )
confint( this_result )
cv( this_result )

grouped_result <-
	svyby( 
		~ adlaoa6p , 
		~ age_category , 
		nps_design , 
		svymean ,
		na.rm = TRUE 
	)
	
coef( grouped_result )
SE( grouped_result )
confint( grouped_result )
cv( grouped_result )
degf( nps_design )
svyvar( ~ adlaoa6p , nps_design , na.rm = TRUE )
# SRS without replacement
svymean( ~ adlaoa6p , nps_design , na.rm = TRUE , deff = TRUE )

# SRS with replacement
svymean( ~ adlaoa6p , nps_design , na.rm = TRUE , deff = "replace" )
svyciprop( ~ trip_this_week , nps_design ,
	method = "likelihood" )
svyttest( adlaoa6p ~ trip_this_week , nps_design )
svychisq( 
	~ trip_this_week + gender , 
	nps_design 
)
glm_result <- 
	svyglm( 
		adlaoa6p ~ trip_this_week + gender , 
		nps_design 
	)

summary( glm_result )
library(srvyr)
nps_srvyr_design <- as_survey( nps_design )
nps_srvyr_design %>%
	summarize( mean = survey_mean( adlaoa6p , na.rm = TRUE ) )

nps_srvyr_design %>%
	group_by( age_category ) %>%
	summarize( mean = survey_mean( adlaoa6p , na.rm = TRUE ) )

