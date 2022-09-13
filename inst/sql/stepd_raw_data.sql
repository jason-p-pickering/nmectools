 SELECT ou.uid,
  p.iso,
  p.startdate,
  p.enddate,
  a.date::date as submission_date,
  b.first_date as first_date,
  ceil(extract(epoch from '${reportExDate}'::timestamp - a.date)/86400) as report_age,
  ceil(extract(epoch from '${reportExDate}'::timestamp - p.enddate::date)/86400) as period_age,
  a.storedby,
  COALESCE(c.count,0) as record_count FROM
  (SELECT periodid, sourceid, storedby,MIN(date) as date
    FROM private.completedatasetregistration_new_records
    WHERE datasetid = (SELECT datasetid from dataset where name = 'Step D Monthly CHW')
    and date <='${reportExDate}'::date
    AND age('${reportExDate}'::date,date)<='1 month'::interval
    GROUP BY periodid, sourceid,storedby ) a
  INNER JOIN (
    SELECT periodid,sourceid,min(date)::date as first_date
    from private.completedatasetregistration_new_records
    WHERE datasetid = (SELECT datasetid from dataset where name = 'Step D Monthly CHW')
    GROUP BY periodid,sourceid
  ) b on b.periodid = a.periodid and b.sourceid = a.sourceid
  LEFT JOIN (
      SELECT sourceid,periodid,count(value)
      from datavalue where dataelementid IN (
        SELECT dataelementid from dataelementgroupmembers
        where dataelementgroupid = (
          (SELECT dataelementgroupid from dataelementgroup where uid = 'RLdFtwhThNQ')
        ) )
      AND  age('${reportExDate}',created)<='1 month'::interval
      GROUP BY sourceid,periodid
    ) c on c.periodid = a.periodid and c.sourceid = a.sourceid
    INNER JOIN _periodstructure p on a.periodid = p.periodid
    INNER JOIN organisationunit ou on a.sourceid = ou.organisationunitid
    ORDER BY ou.uid,p.iso

