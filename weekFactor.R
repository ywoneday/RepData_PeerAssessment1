weekFactor <- function(x) {
  ret = "weekday"
  if(weekdays(as.Date(x)) %in% c("星期六","星期日"))
    ret = "weekend"
  ret
}

