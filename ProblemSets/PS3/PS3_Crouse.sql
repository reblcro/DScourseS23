--create table and import csv data
CREATE TABLE florida_insurance_data (
    policyID INTEGER,
    statecode TEXT,
    county TEXT,
    eq_site_limit REAL,
    hu_site_limit REAL,
    fl_site_limit REAL,
    fr_site_limit REAL,
    tiv_2011 REAL,
    tiv_2012 REAL,
    eq_site_deductible REAL,
    hu_site_deductible REAL,
    fl_site_deductible REAL,
    fr_site_deductible REAL,
    point_latitude REAL,
    point_longitude REAL,
    line TEXT,
    construction TEXT,
    point_granularity INTEGER
);

.mode csv
.import FL_insurance_sample.csv florida_insurance_data

--print first 10 rows
SELECT * FROM florida_insurance_data LIMIT 10;

--list unique counties in dataset
SELECT DISTINCT county FROM florida_insurance_data;

--compute average property appreciation from 2011 to 2012
SELECT AVG(tiv_2012 - tiv_2011) as avg_appreciation FROM florida_insurance_data;

--create frequency table
SELECT construction, COUNT(*) as count, COUNT(*) * 1.0 / (SELECT COUNT(*) FROM florida_insurance_data) as fraction
FROM florida_insurance_data
GROUP BY construction;

