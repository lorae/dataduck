# temporary module defining experimental functions
create_sql_query <- function() {
  query <- "
    WITH age_buckets AS (
        -- Defining the bucket ranges
        SELECT 'Under 16' AS bucket_name, 0 AS lower_bound, 16 AS upper_bound
        UNION ALL
        SELECT '16-17', 16, 18
        UNION ALL
        SELECT '18-22', 18, 23
        UNION ALL
        SELECT '23-29', 23, 30
        UNION ALL
        SELECT '30+', 30, NULL  -- NULL for upper bound as infinity
    )
    
    -- Applying the lookup table to the AGE column
    SELECT 
        data.*, 
        COALESCE(bucket_name, 'Unknown') AS AGE_bucketed
    FROM 
        ipums_bucketed AS data
    LEFT JOIN 
        age_buckets
    ON 
        data.AGE >= age_buckets.lower_bound 
        AND (data.AGE < age_buckets.upper_bound OR age_buckets.upper_bound IS NULL);
    "
}