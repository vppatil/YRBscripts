/*Select records from the percent cover table where the habitat type was GS*/

SELECT TransectID,HabitatType,HabitatTypeLength 
FROM tblVegTransectPctCover 
WHERE HabitatType = 'GS';
