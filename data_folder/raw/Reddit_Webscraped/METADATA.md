**Type**

Type: String

Values: Comment, Post

Description: Specifies whether the entry is a comment or a post.

**Body**

Type: String

Description: The text content of the comment or post.

**Number of Comments**

Type: Integer

Values: Positive number or 0

Description: The number of replies to the comment or post.

**Number of Upvotes**

Type: Integer

Values: Positive number or 0

Description: The number of upvotes the comment or post received.

**Ratio of Upvotes**

Type: Float or NA

Values: Between 0 and 1, or NA if unavailable

Description: The ratio of upvotes to total votes (upvotes + downvotes), if available.

**Submission ID**

Type: String

Description: Unique identifier for the submission (Reddit post) where the comment or post appears.

**Parent ID**

Type: String or NA

Description: Unique identifier for the parent comment if it is a reply, or NA if it's a standalone post.

**URL**

Type: String (URL)

Description: Direct link to the Reddit comment or post.
