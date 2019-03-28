# Array of Things Client

This library serves as the official R client to the [Array of Things API](https://api.arrayofthings.org/).

## Using the Library

This isn't listed with CRAN yet (because it's awful -- I'm not an R developer). You _can_ install it
from GitHub though:

```R
devtools::install_github("UrbanCCD-UChicago/aot-client-r")
```

There are two general types of functions presented: _ls_ and _stat_. You should use the ls functions
to work with the list endpoints, and stat is for details:

- `ls.projects` to get a list of projects
- `ls.nodes` to get a list of nodes
- `ls.sensors` to get a list of sensors
- `ls.observations` to get the observation data
- `stat.project` to get details for a single project
- `stat.node` to get details for a single node
- `stat.sensor` to get details for a single sensor

The _stat_ functions require a unique id for the type of metadata you're looking for -- `slug` for
projects, `vsn` for nodes, and `path` for sensors.

All of the functions allow you to add arbitrary filters/parameters as well:

```R
# sensors onboard node 004
df <- ls.sensors(filters=list(node="004"))

df <- ls.observations(filters=list(
  sensor="metsense.bmp180.temperature",
  timestamp="ge:2018-08-01T00:00:00", # note that the api only contains the past week's data
  timestamp="lt:2018-09-01T00:00:00"  # so your filters will be for obviously later dates here.
))
```
