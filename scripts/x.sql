CREATE TABLE {`O`} AS
SELECT a.{`A_ID`}, b.{`B_ID`},
 CASE
   WHEN ST_CoveredBy(a.geom, b.geom) THEN
    a.geom
   ELSE
    ST_Multi(ST_Intersection(a.geom, b.geom))
   END AS geom
 FROM {`A`} AS a
   INNER JOIN {`B`} AS b
    ON (ST_Intersects(a.geom, b.geom)
      AND NOT ST_Touches(a.geom, b.geom) );
