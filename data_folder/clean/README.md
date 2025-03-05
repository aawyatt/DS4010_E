`MealPlanBySemester.csv` has one row for each student ID in the data and one column for each semester in the data we received from ISU Dining. For every ID, the semester contains the Meal Plan for that semester or NA if no Meal Plan was purchased that semester. If a student purchased more than one meal plan that term, the plans are separated by commas. 

`RoomLocationBySemester.csv` has one row for each student ID in the data and one column for each semester in the data we received from ISU Dining. For every ID, the semester contains the room location of that student for that semester or NA if the student did not purchase a meal plan that semester.


`CleanedReviews.csv` has one row per review found from Reddit, Google Reviews, or our custom survey. In each row, there is:

  - **Rating**: Rating from the review. Reddit posts were ran through a sentiment analysis algorithm to get rating(see `RedditGoogleJoiner.py` for more details), Google reviews were scraped with a given rating out of 5 already, and our custom survey was converted to be out of 5.

- **Body**: Body of the review if the review gives any reasoning for their score.
- **Dining Hall**: Which dining hall in particular they are refering to.
- **reviewUrl**: Link to where the data came from.
-  **Date**: When the review was posted.

`CleanedReviews.csv` has 1 row per residence area on campus and one column for each semester starting in Spring 2021 and ending in Spring 2025. Each cell within this file has the total population of students residing in that hall in the given term.

`CleanDiningData.csv` has all the meal plan information and housing location semester-wise by student. For each student, it stores a term, meal plan, housing location, and random unique ID.

`CurrentDiningData.csv` has all the meal plan information and housing location semester-wise by student for the currently continuing plans (Does not have the discontinued plans). For each student, it stores a term, meal plan, housing location, and random unique ID.
