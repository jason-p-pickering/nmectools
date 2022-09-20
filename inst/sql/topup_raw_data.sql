SELECT ou.name,
ps.iso as week_value,
p.startdate,
'${reportExDate},'::date - p.startdate  as period_age,
y.record_age,
ou.uid as orgunit_uid,
y.submission_date from
( SELECT datasetid, periodid, sourceid ,date as submission_date,
'${reportExDate}'::timestamp  - date as record_age
from
(
SELECT datasetid, periodid, sourceid, MAX(date) as date from (
SELECT a.datasetid, a.periodid,a.sourceid,a.date,b.exists
from private.completedatasetregistration_new_records a
LEFT JOIN (
SELECT  DISTINCT datasetid, periodid,sourceid, 1 as exists
from private.completedatasetregistration_new_records
WHERE ('${reportExDate}'::timestamp, - date) > '7 days'::interval
and date <= '${reportExDate}'::date
) b on a.datasetid = b.datasetid
AND a.periodid = b.periodid
and a.sourceid = b.sourceid
where a.datasetid = 69047
and a.date <= '${reportExDate}'::date
and '${reportExDate}'::timestamp - date  <= '7 days'::interval
) as foo
WHERE exists IS NULL
GROUP BY datasetid, periodid, sourceid
)  as x )  as y
INNER JOIN organisationunit ou on y.sourceid = ou.organisationunitid
INNER JOIN period p on y.periodid = p.periodid
INNER JOIN _periodstructure ps on p.periodid = ps.periodid
WHERE p.startdate < '${reportExDate}'::date
ORDER BY ou.name, ps.iso DESC
