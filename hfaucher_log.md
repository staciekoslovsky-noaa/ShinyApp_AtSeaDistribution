# Internship Log: Haley Faucher

## June 1st, 2026
### 11:00 - First Check in with Stacie
- Talked about internship schedule, Stacie will send calendar invites for coworking time and other meetings
- Need to update goals on project document
### 12:00 - Lapenta Orientation
- Introduction to different line office structures
- Mostly administrative stuff, like housing and travel reimbursements

## June 2nd, 2026
### 9:00 - Goals
- Added goals to project document
### 12:00 - Coworking
- Talked about goals for internship, with some updates
- Discussed code review and update of work plan (aim to be complete by June 3rd)
### 14:00 - Code Review (for AtSeaDistribution)
- Within app.R: 
  - redo the logic for species list with a for-loop
    - Be careful, as some species could have more data files than others with different spatial representations
  - Separate UI and server
  - Potentially make the sidebar into a horizontal navigation bar at the top?
  - Remove color palettes, but keep option to make greyscale (unnecessary)
  - Make sure shapes and shapefiles are top layer always on leaflet. Is the ability to disable the layers necessary?
- Within helper_functions.R:
  - Find a better place to store the text strings for the information tabs
- All other fixes that are mentioned in project document already

## June 3rd, 2026
### 9:00 - Blueprint
- Wrote blueprint for first 4 weeks based on code review
### 12:00 - Coast Survey - Data Driven Future (Lorraine Robidoux)
- Deputy Director of the Office of Coast Survey, which is a subset of the NOS
- Navigation products like the ENC (Electronic Navigational Chart) instead of the rasters aligns to mapping in AtSeaDistribution application
### 13:00 - Project Meeting
- With the for-loop approach, other species can have spatio-temporal representation,
  - Paul will provide basic data to test with
  - Try to have some sort of standardization with the data for easier handling
- A couple of information interviews: Erin, Ben, Josh