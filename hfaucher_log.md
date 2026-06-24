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

## June 4th, 2026
### 9:30 - Planning and Programming
- Edited the blueprint a bit, and made notes for the next steps
- Separated the UI and the server and moved the old helper_functions and custom_area_analysis into the global.R for now. 
  - Waiting for coworking to merge into main
- Organized packages on global.R
### 12:00 - How to get the most of the internship
- Make networking a priority, and reach out to people even to talk about non-work related subjects
### 13:00 - Coworking
- Working with GitHub and going over pull request
  - Merged
- Stacie told me about Positron/VSCode so spent most of the time trying to get that working
- Started working on disbaled the generating custom area analysis button until a shapefile(s) has been uploaded
### 14:00 - Integrated Data Analysis Working Group
- Introduced myself
- Listened to others on how I could contribute to their projects

## June 5th, 2026
### 9:00 - Setting Up and Programming
- Got VSCode to work well on my laptop
  - Had to add a load statement on global because VSCode doesn't have sessions that save variables
- Made a branch to disable the generate button until a valid file is uploaded
 - This caused an error as I was testing with an invalid file
  - Solved this by adding an else statement
- Started cleaning the code according to the R linter
  - Only got through UI, but the other ones should *hopefully* be quicker. 
### 12:00 - From the GCRL SFP to Board-Certified Fish Veterinarian (Johnny Shelley, NMFS SIP)
- Mostly about his pathway
  - Emphasized networking and extracurricular research/academics
  
## June 8th, 2026
### 9:00 - Programming
- Merged in pr for disabling the generate analysis button
- Cleaned global.R by removing functions and variables that aren't used
### 12:00 - Domestic fisheries management 101 (Deb Lambert, NMFS)
- Presentation on the three pillars of fisheries service
- Described stock assessment, and saw some parallels to our project
### 13:00 - Coworking
- Worked on cleaning server.R, but messed up at some point so had to revert my changes
  - This should be done by tomorrow, and I should be able to tackle the color palette removal
### 15:00 - Global Monitoring Laboratory "Taking the Pulse of the Planet" (Vanda Grubisic)
- Talked about the research themes of the laboratory
- Described data products, which was cool to hear about

## June 9th, 2026
### 9:00 - Programming
- Cleaned server.R
- Removed all lintr comments from all three files once everything was cleaned
- Waited for Stacie to approve, then merged
### 12:00 - Satellite Observations of Severe Storms (Dan Lindsey, NESDIS)
- Talked about current satellites (GOES-East and GOES-West)
  - Current version (GOES-19 and GOES-16)
  - Future improvements (increased resolution)
### 13:00 - Programming
- Removed color palette options
  - Kept a checkbox for greyscale
- Resolved merge conflicts with this PR
### 15:00 - National Data Buoy Center "Guardians of the Pelagic Sea" (Bill Burnett)
- Described the four different types of data buoys, as well as uncrewed systems and Voluntary Observing Ships
### 16:30 - Coworking
- Discussed looking more into the files and why we have POPhex_MCMC and POPhexagons_sf
  - Going to look more into this tomorrow
- Decided to not meet with Paul tomorrow, as there is not much visual changes to show

## June 10th, 2026
### 9:00 - Solo Work
- Trying to understand the data pipeline a little more
  - Looking through variables in RStudio environment
  - Coming up with a way to change the pipeline to be a bit more efficient
### 12:00 - Global Ocean Monitoring and Observation in NOAA and Beyond (Jess Snowden)
- A good seminar building off of the National Data Buoy Center talk from yesterday
- Also mentioned that you DON'T need a PHD to succeed at NOAA.
### 13:00 - Coworking
- Still working through pipeline
  - Stacie provided me with a code snippet that should make it possible to only load in files as needed.

## June 11th, 2026
### 9:00 - Programming
- Outlined changes to be made to data pipeline
  - Ran into issue with CSV formatting, had to change it
### 13:00 - Coworking
- Did a small demo for Stacie to see if load speed was alright
- After some feedback, moved scale to bottom left, added italicized latin name to map header, and removed the layer controls
  - This may need to be added back after, as shapefiles may not be deleted by trash icon
### 15:00 - Data Management for the Rest of Us (Stacie Koslovsky, AFSC, NMFS, Seattle)
- Some good info on data management (good standards for my work)
- Interesting stuff about detection models
  - Could be a way to expand on my internship contribution

## June 12th, 2026
### 9:00 - Programming
- Merged data-pipeline pr
- Began working on draw tools functionality
  - Had to add a raw species data reactive for calculations
  - Added helper function that generates histogram for shapes and shapefiles
- Currently, rectangle tool works as expected (all calculations correct)
  - Circle does not, as we may need to change the way that the data is gathered from the circle
### 12:00 - Early Career Professional Perspective (Jessica Bunker, Norman OK)
- Talked about CIWRO
- Described ways to grow my career now:
  - Finding a mentor
  - Asking people in my desired field to review my resume
  - Picking a specialty
### 13:15 - Intern Orientations
- Completed Intern and Safety Orientations for UW housing

## June 15th, 2026
### 9:30 - Lapenta Internship Icebreaker
- Got introduced to Cathy Lapenta, Tim Walsh, Ray Tanabe, Dr. Jon Har, Dr. Marian Westly, and Commander Kevin Doremus
### 13:30 - Coworking
- Mostly talking about tools
  - Circles were being calculated with a bounding box
    - This was changed, now has a more accurate calculation
  - Multiple shapes can be added, but only one has current calculation in shape generation tab
    - Need to find a way to clear other shapes, or remove option to add more until it is deleted.

## June 16th, 2026
### 9:00 - Programming
- Found "singleFeature = TRUE" for only added one shape at a time
  - Now when another one is added, the first feature is removed, and the analysis updates for the most recent shape
- Found that adding a shape and then a species doesn't work, so added in code so this works.
- Added edge case in generate_custom_analysis that clears table and histogram if a shape is removed
### 12:00 - NWS Deputy Director Michelle Mainelli Talks with Interns!
- Got a MS and MBA while working for NOAA
  - An option for me
### 13:00 - Coworking
- Decided to add an option for going to specified coordinates
- Updated blueprint and tickets
- Planned meeting with Paul
### 15:00 - NOAA Technology Partnerships Office (Mike Kruk)
- About the TPO and its different directives
  - SBIR
  - Technology Transfer
- Also mentioned some current partnerships they have (good job opportunities)

## June 17th, 2026
### 12:15 - Programming
- Merged draw tools pull request
- Worked on getting shapefile area analysis to work but ran into some bugs
  - Shapefile doesn't update after other fields are edited
  - You cannot remove the shape file
  - Both a shapefile and a drawn shape are visible on the map at the same time
- Started fixing those bugs
  - Allowing shapefile analysis to update if a variable changes
### 13:00 - Project Meeting
- Demo was good, but should look into the collapsing of the tabs again (you have to scroll down quite far to see the analysis)
- Need a way to standardize data, hopefully in a group with the data science stewards
### 13:45 - Programming
- Fixing the rest of the shapefile bugs
  - Made it so only shapefile or shape can be viewed with Javascript call
  - Added a remove button
- Made PR
### 15:00 - Space Weather 101 (Yaireska (Yari) Collado-Vega)
- Intro to Space Weather Prediction Center and Office of Space Weather Observations

## June 18th, 2026
### 9:00 - Programming
- Merged shapefile analysis pr
- Worked on getting file attributes for shapefile download
  - Fixed associated bugs like the download button being available at any time
- Forgot to make a branch so reverted commit and then branched
### 12:00 - Dr. Uccellini Speaks to Lapenta Interns (Weather Ready Nation)
- The steps to building a weather ready nation, and some case studies
### 13:00 - Coworking
- Reviewed and merged pr
- Did some chatting about changes in issues section of GitHub, all resolved now!!

## June 22nd, 2026
### 9:00 - ID Badge pickup
- Picked up ID badge 
### 10:00 - NOAA account and laptop setup
- Set windows and updated computer
- Set NOAA account up after some difficulty
### 11:00 - Solo Set Up
- Read through old gmail
- Emailed Dr. Roohr, so I can be added to the seminar calendar
### 12:30 - Trainings
- Read CLC Welcome Letter
- Did CSAT training
### 14:00 - Installing Services
- Had to contact help desk to install R and Git onto laptop
- Downloaded Positron wihtout any issues
### 15:30 - Programming
- Began adding option for preloaded shapefiles, testing with import
  - Button is there, but there is no server code yet.

## June 23rd, 2026
### 8:00 - Programming
- Added server logic to preloaded shapefiles, with a reactiveVal that holds the path of the file
  - Removed for-loop logic, because only one shapefile can be on the map at all times
### 9:00 - Howard Diamond (OAR) - OAR's Air Resources Laboratory Intro
- Talked about different divisions, and one of the lab's products
  - HYSPLIT
### 10:00 - Programming
- Fixed some more small bugs in the preloaded shapefiles, making sure everything is removed when one is chosen
- Made pr for preloaded shapefiles
- Added an output underneath the map that shows the cell area (static now, but with changing data types, we will see how it changes)
  - Made pr
### 13:00 - Programming
- Worked on new zoom-to feature
  - The center is set to 208 which was giving some issues 
    - Thought about cleaning data as hexagons were also applied around 208
  - Got latitude to work with a reactive, but longitude was not working as the centering issue was weird
### 14:00 - Walk with Paul
- Walked around campus a bit, and talked about different calculations we will have to do with the different data types
### 15:00 - Programming
- Figured out the longitude issue (typo :/)
  - Made draft pr, as I want Stacie's opinion on the aesthetics of the UI