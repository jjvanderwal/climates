daily2monthly<-function(daily_data, time, origin_in, cells=NULL)
{
  if (is.null(cells)) cells<-ncol(daily_data)
  time<-chron(time,out.format=c(dates="year-m-day"),origin=origin_in)
  daily_data<-zoo(daily_data,time)
  daily_data<-aggregate(daily_data, as.yearmon, mean)
  monthlyData<-t(data.matrix(fortify.zoo(daily_data),cells)[1:12,2:(cells+1)])
  return(monthlyData)
}