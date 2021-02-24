@val external document: {..} = "document"
@val external window: {..} = "window"

module Post = {
  type t = {
    title: string,
    author: string,
    text: array<string>,
  }

  let make = (~title, ~author, ~text) => {title: title, author: author, text: text}
  let title = t => t.title
  let author = t => t.author
  let text = t => t.text
}

let posts = [
  Post.make(
    ~title="The Razor's Edge",
    ~author="W. Somerset Maugham",
    ~text=[
      "\"I couldn't go back now. I'm on the threshold. I see vast lands of the spirit stretching out before me,
    beckoning, and I'm eager to travel them.\"",
      "\"What do you expect to find in them?\"",
      "\"The answers to my questions. I want to make up my mind whether God is or God is not. I want to find out why
    evil exists. I want to know whether I have an immortal soul or whether when I die it's the end.\"",
    ],
  ),
  Post.make(
    ~title="Ship of Destiny",
    ~author="Robin Hobb",
    ~text=[
      "He suddenly recalled a callow boy telling his tutor that he dreaded the sea voyage home, because he would have
        to be among common men rather than thoughtful acolytes like himself. What had he said to Berandol?",
      "\"Good enough men, but not like us.\"",
      "Then, he had despised the sort of life where simply getting from day to day prevented a man from ever taking
        stock of himself. Berandol had hinted to him then that a time out in the world might change his image of folk
        who labored every day for their bread. Had it? Or had it changed his image of acolytes who spent so much time in
        self-examination that they never truly experienced life?",
    ],
  ),
  Post.make(
    ~title="A Guide for the Perplexed: Conversations with Paul Cronin",
    ~author="Werner Herzog",
    ~text=[
      "Our culture today, especially television, infantilises us. The indignity of it kills our imagination. May I propose a Herzog dictum? Those who read own the world. Those who watch television lose it. Sitting at home on your own, in front of the screen, is a very different experience from being in the communal spaces of the world, those centres of collective dreaming. Television creates loneliness. This is why sitcoms have added laughter tracks which try to cheat you out of your solitude. Television is a reflection of the world in which we live, designed to appeal to the lowest common denominator. It kills spontaneous imagination and destroys our ability to entertain ourselves, painfully erasing our patience and sensitivity to significant detail.",
    ],
  ),
]

let postHeading = (heading: string) => {
  let headingTag = document["createElement"]("h2")
  headingTag["classList"] = "post-heading"
  headingTag["innerText"] = heading
  headingTag
}

let postSubHeading = (subHeading: string) => {
  let subHeadingTag = document["createElement"]("h3")
  subHeadingTag["innerText"] = subHeading
  subHeadingTag
}

let postParagraph = (paraText: string) => {
  let paraTag = document["createElement"]("p")
  paraTag["classList"] = "post-text"
  paraTag["innerText"] = paraText
  paraTag
}

let appendId = (id, index) => `${id}-${Belt.Int.toString(index)}`

// let postId = index => `block-${Belt.Int.toString(index)}`
// let btnId = index => `block-delete-${Belt.Int.toString(index)}`

let postBtn = index => {
  // <button id="block-delete-0" class="button button-danger">Remove this post</button>
  let btn = document["createElement"]("button")
  btn["setAttribute"]("id", appendId("block-delete", index))
  btn["classList"] = "button button-danger"
  btn["innerText"] = "Remove"
  btn
}

let postDiv = (index: int, post: Post.t) => {
  let mainDiv = document["createElement"]("div")
  mainDiv["classList"] = "post"
  mainDiv["setAttribute"]("id", appendId("block", index))
  mainDiv["appendChild"](postHeading(post.title))
  mainDiv["appendChild"](postSubHeading(post.author))
  Belt.Array.forEach(post.text, text => {
    mainDiv["appendChild"](postParagraph(text))
  })
  mainDiv["appendChild"](postBtn(index))
  mainDiv
}

let deletePara = title => {
  let paraTag = document["createElement"]("p")
  paraTag["classList"] = "text-center"
  let preText = document["createTextNode"]("This post from ")
  let empText = document["createElement"]("em")
  empText["innerText"] = title
  let postText = document["createTextNode"](" will be permanently removed in 10 seconds.")
  paraTag["appendChild"](preText)
  paraTag["appendChild"](empText)
  paraTag["appendChild"](postText)
  paraTag
}

let recoverDiv = (index, post: Post.t) => {
  let deletedDiv = document["createElement"]("div")
  deletedDiv["classList"] = "post-deleted pt-1"
  deletedDiv["setAttribute"]("id", appendId("block", index))
  deletedDiv["appendChild"](deletePara(post.title))

  let flexDiv = document["createElement"]("div")
  flexDiv["classList"] = "flex-center"

  let restoreBtn = document["createElement"]("button")
  restoreBtn["setAttribute"]("id", appendId("block-restore", index))
  restoreBtn["classList"] = "button button-warning mr-1"
  restoreBtn["innerText"] = "Restore"

  let delImmBtn = document["createElement"]("button")
  delImmBtn["setAttribute"]("id", appendId("block-delete-immediate", index))
  delImmBtn["classList"] = "button button-danger"
  delImmBtn["innerText"] = "Delete Immediately"

  flexDiv["appendChild"](restoreBtn)
  flexDiv["appendChild"](delImmBtn)

  let progressDiv = document["createElement"]("div")
  progressDiv["classList"] = "post-deleted-progress"

  deletedDiv["appendChild"](flexDiv)
  deletedDiv["appendChild"](progressDiv)

  deletedDiv
}

Belt.Array.forEachWithIndex(posts, (index, post) => {
  document["body"]["appendChild"](postDiv(index, post))
  document["body"]["appendChild"](recoverDiv(index, post))
})
