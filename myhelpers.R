# global vars
numVars <- c("Apps",
             "ScreenTime_hr",
             "AppUsageTime_hr",
             "DataUsage_MB",
             "BatteryUsage_mAh")

#read in full data ONE TIME ONLY
dt <- readr::read_csv("user_behavior_dataset.csv",col_names = TRUE)

dt <- dt |> mutate(across("User ID",as.character))
names(dt) <- c(
  "ID",
  "Model",
  "OS",
  "AppUsageTime_min",
  "ScreenTime_hr",
  "BatteryUsage_mAh",
  "Apps",
  "DataUsage_MB",
  "Age",
  "Gender",
  "UsageClass"
)
#ages binned by 5, from min 18 through max 59
dt$AgeGroup <- cut(
  dt$Age,
  breaks=seq(15,60,by=5)
)

#app usage converted to minutes
dt$AppUsageTime_hr <- round(dt$AppUsageTime_min/60,digits=1)

#factor all character strings
dt$Gender <- factor(dt$Gender)
dt$Model <- factor(dt$Model)
dt$OS <- factor(dt$OS)
dt$UsageClass <- factor(dt$UsageClass,
                          1:5,
                          labels=c(
                            'Light 1',
                            'Low 2',
                            'Average 3',
                            'High 4',
                            'Extreme 5')
)

#only keep the columns that I want
dt <- 
  dt |> 
  select(
    ID,
    Model,
    OS,
    Gender,
    AgeGroup,
    Apps,
    ScreenTime_hr,
    AppUsageTime_hr,
    DataUsage_MB,
    BatteryUsage_mAh,
    UsageClass
  )