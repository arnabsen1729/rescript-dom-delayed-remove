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

let getIndexFromInd = index => {
  let splittedArray = Js.String.split("-", index)
  let id = Belt.Array.getExn(splittedArray, Belt.Array.length(splittedArray) - 1)
  id
}

let appendId = (id, index) => `${id}-${index}`

// returns a DOM element for the heading
let postHeading = (~heading: string, ~className: string) => {
  let headingTag = document["createElement"]("h2")
  headingTag["classList"] = className
  headingTag["innerText"] = heading
  headingTag
}

// returns a DOM element for the heading
let postSubHeading = (subHeading: string) => {
  let subHeadingTag = document["createElement"]("h3")
  subHeadingTag["innerText"] = subHeading
  subHeadingTag
}

// returns a DOM element for the paragraph
let postParagraph = (~paraText: string, ~className: string) => {
  let paraTag = document["createElement"]("p")
  paraTag["classList"] = className
  paraTag["innerText"] = paraText
  paraTag
}

// Section of the recover div containing the title text
let deletePara = title => {
  let paraTag = document["createElement"]("p")
  paraTag["classList"] = "text-center"
  let preText = document["createTextNode"]("This post from ")
  let empText = document["createElement"]("em")
  empText["innerText"] = title
  let postText = document["createTextNode"](" will be permanently removed in 10 seconds.")
  paraTag["appendChild"](preText)->ignore
  paraTag["appendChild"](empText)->ignore
  paraTag["appendChild"](postText)->ignore
  paraTag
}

// Event to restore the post
let restoreFn = (event, originalDiv, timeoutID) => {
  Js.log("Restore event called...")

  // clearing the timeout of post deletion since the post is restored
  window["clearTimeout"](timeoutID)->ignore

  // Parent div
  let recoverElem = event["path"][2]
  recoverElem["replaceWith"](originalDiv)
}

// Event to restore the post
let deleteFn = (event, timeoutID) => {
  Js.log("Delete event called...")

  // clearing the timeout of post deletion since the post is deleted
  window["clearTimeout"](timeoutID)->ignore

  let recoverElem = event["path"][2]

  // removing the entire DOM element
  recoverElem["remove"]()
}

let recoverDiv = (index: string, title: string, originalDiv) => {
  let deletedDiv = document["createElement"]("div")
  deletedDiv["classList"] = "post-deleted pt-1"
  deletedDiv["setAttribute"]("id", appendId("block", index))->ignore
  deletedDiv["appendChild"](deletePara(title))->ignore

  let flexDiv = document["createElement"]("div")
  flexDiv["classList"] = "flex-center"

  let restoreBtn = document["createElement"]("button")
  restoreBtn["setAttribute"]("id", appendId("block-restore", index))->ignore
  restoreBtn["classList"] = "button button-warning mr-1"
  restoreBtn["innerText"] = "Restore"

  let timeoutID = window["setTimeout"](() => {
    Js.log("Timeout")
    document["getElementById"](deletedDiv["id"])["remove"]()
  }, 10000)

  restoreBtn["addEventListener"]("click", event => {
    restoreFn(event, originalDiv, timeoutID)
  })->ignore

  let delImmBtn = document["createElement"]("button")
  delImmBtn["setAttribute"]("id", appendId("block-delete-immediate", index))->ignore
  delImmBtn["classList"] = "button button-danger"
  delImmBtn["innerText"] = "Delete Immediately"
  delImmBtn["addEventListener"]("click", event => {deleteFn(event, timeoutID)})->ignore

  flexDiv["appendChild"](restoreBtn)->ignore
  flexDiv["appendChild"](delImmBtn)->ignore

  let progressDiv = document["createElement"]("div")
  progressDiv["classList"] = "post-deleted-progress"

  deletedDiv["appendChild"](flexDiv)->ignore
  deletedDiv["appendChild"](progressDiv)->ignore

  deletedDiv
}

let removeFn = event => {
  Js.log("Remove event called...")
  let divElem = event["path"][1]
  let title = divElem["childNodes"][0]["innerText"]
  let index = getIndexFromInd(divElem["id"])
  let recoverElem = recoverDiv(index, title, divElem)
  divElem["replaceWith"](recoverElem)
}

let postBtn = (index: string) => {
  let btn = document["createElement"]("button")
  btn["setAttribute"]("id", appendId("block-delete", index))->ignore
  btn["classList"] = "button button-danger"
  btn["innerText"] = "Remove"
  btn["addEventListener"]("click", removeFn)->ignore
  btn
}

let postDiv = (index: string, post: Post.t) => {
  let mainDiv = document["createElement"]("div")
  mainDiv["classList"] = "post"
  mainDiv["setAttribute"]("id", appendId("block", index))->ignore
  mainDiv["appendChild"](postHeading(~heading=post.title, ~className="post-heading"))->ignore
  mainDiv["appendChild"](postSubHeading(post.author))->ignore
  Belt.Array.forEach(post.text, text => {
    mainDiv["appendChild"](postParagraph(~paraText=text, ~className="post-text"))
  })
  mainDiv["appendChild"](postBtn(index))->ignore
  mainDiv
}

Belt.Array.forEachWithIndex(posts, (index, post) => {
  document["body"]["appendChild"](postDiv(Belt.Int.toString(index), post))
})
