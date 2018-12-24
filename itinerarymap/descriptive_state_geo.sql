with stategeo([state],lon,lat) 
as
(
select [state] as [state],Avg(long)as long,Avg(lat) as lat 
from air_weather_geo 
where [state] is not null 
group by [state]
union 
select [state_arr] as [state],Avg(long_arr) as long,Avg(lat_arr) as lat
from air_weather_geo  
where [state_arr] is not null 
group by [state_arr]
) 
select [state],AVG(lon)as lon,AVG(lat)as lat 
from stategeo 
group by [state];








