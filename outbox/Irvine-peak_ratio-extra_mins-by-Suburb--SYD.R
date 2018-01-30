library(data.table)
library(grattanRoadCongestion2017)
library(magrittr)
library(grattanCharts)

free_flow_by_ROUTE_ID <-
  unique(traffic_data[, .(ROUTE_ID, free_flow)])

public_holidays <-
  traffic_data %>%
  .[(public_holiday)] %>%
  .[, .(Date = as.Date(time))] %$%
  unique(Date)

orig_Suburb_in_csv <-
  fread("Irvine-peak_ratio-extra_mins-by-Suburb--SYD.csv") %>%
  .[, I := .I]


peak_summary_by_ROUTE_ID_Date_AMPM %>%
  .[AM == "AM"] %>%
  .[Date %notin% public_holidays] %>%
  .[, Wkday := weekdays(Date)] %>%
  .[Wkday %notin% c("Saturday", "Sunday")] %>%
  Suburbs_by_ROUTE_ID[., on = "ROUTE_ID"] %>%
  City_by_ROUTE_ID[., on = "ROUTE_ID"] %>%
  free_flow_by_ROUTE_ID[., on = "ROUTE_ID"] %>%
  .[City == "Sydney"] %>%
  .[dest_Suburb == "CBD"] %>%
  .[, .(free_flow_mins = first(free_flow),
        avg_morning_peak = mean(peak_duration),
        avg_ratio = mean(peak_ratio - 1),
        sd_peak_duration = sd(peak_duration),
        P20 = quantile(peak_duration, probs = 0.2),
        P80 = quantile(peak_duration, probs = 0.8)),
    keyby = "orig_Suburb"] %>%
  .[order(-avg_ratio)] %>%
  fwrite("Irvine-peak_ratio-extra_mins-by-Suburb--SYD.csv")

peak_summary_by_ROUTE_ID_Date_AMPM %>%
  .[AM == "AM"] %>%
  .[Date %notin% public_holidays] %>%
  .[, Wkday := weekdays(Date)] %>%
  .[Wkday %notin% c("Saturday", "Sunday")] %>%
  Classification_by_ROUTE_ID[., on = "ROUTE_ID"] %>%
  .[Classification != "Interstate freight"] %>%
  Suburbs_by_ROUTE_ID[., on = "ROUTE_ID"] %>%
  City_by_ROUTE_ID[., on = "ROUTE_ID"] %>%
  free_flow_by_ROUTE_ID[., on = "ROUTE_ID"] %>%
  .[City == "Melbourne"] %>%
  .[, .(free_flow = first(free_flow),
        avg_trip_time_in_am_peak = mean(peak_duration),
        avg_pc_increase_travel_time = mean(peak_ratio - 1),
        sd_peak_duration = sd(peak_duration),
        P20 = quantile(peak_duration, probs = 0.2),
        P80 = quantile(peak_duration, probs = 0.8)),
    keyby = c("orig_Suburb", "dest_Suburb")] %>%
  .[order(-avg_trip_time_in_am_peak)] %>%
  fwrite("Melbourne-trip-time.csv")


