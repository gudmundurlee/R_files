
DECLARE @start_date DATE = '2019-01-01',
		@end_date DATE = '2022-12-31'

 ;WITH dates AS (
	SELECT 
		id AS item_id,
		dt.last_day_month,
		dt.yr,
		dt.mth
	FROM dbo.items i
	CROSS JOIN (
		SELECT 
			MAX(date) AS last_day_month,
			YEAR(date) AS yr,
			MONTH(date) AS mth
		FROM dbo.date_table
		WHERE date BETWEEN @start_date AND @end_date
		GROUP BY YEAR(date), MONTH(date)
	) dt
 ),
 
 histories_sale AS (
 
    SELECT 
        hs.item_id,
        MIN(hs.history_date) min_date,
        YEAR(hs.history_date) AS yr,
        MONTH(hs.history_date) AS mth,
        SUM(hs.value) AS [value]
    FROM dbo.histories_sale AS hs
    --WHERE hs.item_id = 15
    GROUP BY hs.item_id, YEAR(hs.history_date), MONTH(hs.history_date)
 )
    SELECT --TOP 100 
        d.item_id,
		ROW_NUMBER() OVER(PARTITION BY d.item_id ORDER BY d.yr, d.mth) AS time,
		d.yr,
        d.mth,
		--d.last_day_month,
		--hs.min_date,
		--(SELECT TOP 1 min_date FROM histories_sale WHERE item_id = d.item_id ORDER BY min_date) AS start,
		YEAR((SELECT TOP 1 min_date FROM histories_sale WHERE item_id = d.item_id ORDER BY min_date)) AS start_yr,
		MONTH((SELECT TOP 1 min_date FROM histories_sale WHERE item_id = d.item_id ORDER BY min_date)) AS start_mth,
		--CASE WHEN d.last_day_month >= (SELECT TOP 1 min_date FROM histories_sale WHERE item_id = d.item_id ORDER BY min_date) THEN 1 ELSE 0 END,
		CAST(ISNULL(hs.value,0) AS INT) AS sale		
    FROM dates d
		LEFT OUTER JOIN histories_sale hs ON d.item_id = hs.item_id AND d.yr = hs.yr AND d.mth = hs.mth
	WHERE 
		d.last_day_month >= (SELECT TOP 1 min_date FROM histories_sale WHERE item_id = d.item_id ORDER BY min_date)
	ORDER BY d.item_id, d.yr, d.mth
    --GROUP BY item_id