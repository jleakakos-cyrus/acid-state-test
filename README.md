In GHCI:

acid <- openLocalState emptyPosts

query' acid GetPosts

update' acid (AddPost "TitleText" "ContentText")
