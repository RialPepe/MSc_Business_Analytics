-- Data visualization and management individual assignment:

use chinook;

-- 1. Show the artists with number of albums they have in descending order.

-- Seleccionas la columna de Name (tabla de artist) y creas una nueva que sea la suma de AlbumId entre si mismo (tabla de albums),
-- juntas por album para que no haya un Nombre con 0 Albums, agrupas por nombre y ordenas por el total de forma descendiente.alter.

-- Select the Name column (artist table) and create a new column that is the sum of AlbumId divided by itself (albums table), 
-- join by album so that there is no Name with 0 Albums, group by name and sort by total descending order.alter.

SELECT 
	artist.Name, 
    SUM(album.AlbumId/album.AlbumId) AS totalalbums
FROM 
	album
LEFT JOIN 
	artist ON artist.ArtistId = album.ArtistId
GROUP BY 
	Name
ORDER BY 
	totalalbums DESC;


-- 2. Show name and address of customer with highest total sales across the complete invoice table.

-- Same as in the previous exercise but now instead of selecting just the name, now we concatenate the first and last name.

SELECT 
    concat_ws("", customer.FirstName, customer.LastName) full_name,
	customer.Address,
    SUM(invoice.Total) AS Total
FROM 
	customer
LEFT JOIN 
	invoice ON invoice.CustomerId = customer.CustomerId
GROUP BY 
	full_name
ORDER BY 
	Total DESC;


-- 3. Find the name of the tracks that occur most frequently in playlist TV Shows and playlist 90’s music.

-- Same as in the previous exercise but now we only want the first row so we use the limit function.
SELECT
	track.name,
    COUNT(playlisttrack.PlaylistId WHEN ) AS most_popular
FROM 
	track
RIGHT JOIN 
	playlisttrack ON track.TrackId = playlisttrack.PlaylistId
GROUP BY 
	track.Name
LIMIT 1;


SELECT * 
FROM playlisttrack;






-- 4. Show name of top 10 tracks by number of times they are present in different playlists 
-- i.e., a track is number 1 if it is present in maximum number of playlists.

-- -- Same as in the previous exercise but now we only want the first 10 rows so we change the limit function from 1 to 10.

SELECT
	track.name,
    COUNT(playlisttrack.PlaylistId) AS most_popular
FROM 
	track
RIGHT JOIN 
	playlisttrack ON track.TrackId = playlisttrack.PlaylistId
GROUP BY 
	track.Name
ORDER BY most_popular DESC
LIMIT 10;


-- 5. Show the countries that the customers belong to with number of customers in each country listed in ascending order.
SELECT 
	Country,
    SUM(customer.CustomerId/customer.CustomerId) AS total_customers
FROM 
	customer
GROUP BY 
	Country
ORDER BY 
	total_customers;

-- 6 Use the database to analyze the relationship between customers, employees, playlists, tracks etc. 
-- and find 3 insights that should be of concern to the managers of a music company looking to expand their operations.

-- 1.- 
SELECT
	genre.name,
    COUNT(track.TrackId) AS most_popular
FROM 
	genre
RIGHT JOIN 
	track ON genre.GenreId = track.GenreId
GROUP BY 
	genre.Name
ORDER BY most_popular DESC
LIMIT 10;


-- 2.- 
SELECT 
	genre.Name,
    SUM(invoice.Total) AS Total
FROM 
	genre
RIGHT JOIN 
	track ON track.GenreId = genre.GenreId
RIGHT JOIN 
	invoiceline ON invoiceline.TrackId  track.TrackId
GROUP BY 
	genre.Name
ORDER BY 
	Total DESC;