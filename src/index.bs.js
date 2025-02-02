// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Belt_Array from "bs-platform/lib/es6/belt_Array.js";
import * as Caml_array from "bs-platform/lib/es6/caml_array.js";

function make(title, author, text) {
  return {
          title: title,
          author: author,
          text: text
        };
}

function title(t) {
  return t.title;
}

function author(t) {
  return t.author;
}

function text(t) {
  return t.text;
}

var Post = {
  make: make,
  title: title,
  author: author,
  text: text
};

var posts = [
  {
    title: "The Razor's Edge",
    author: "W. Somerset Maugham",
    text: [
      "\"I couldn't go back now. I'm on the threshold. I see vast lands of the spirit stretching out before me,\n    beckoning, and I'm eager to travel them.\"",
      "\"What do you expect to find in them?\"",
      "\"The answers to my questions. I want to make up my mind whether God is or God is not. I want to find out why\n    evil exists. I want to know whether I have an immortal soul or whether when I die it's the end.\""
    ]
  },
  {
    title: "Ship of Destiny",
    author: "Robin Hobb",
    text: [
      "He suddenly recalled a callow boy telling his tutor that he dreaded the sea voyage home, because he would have\n        to be among common men rather than thoughtful acolytes like himself. What had he said to Berandol?",
      "\"Good enough men, but not like us.\"",
      "Then, he had despised the sort of life where simply getting from day to day prevented a man from ever taking\n        stock of himself. Berandol had hinted to him then that a time out in the world might change his image of folk\n        who labored every day for their bread. Had it? Or had it changed his image of acolytes who spent so much time in\n        self-examination that they never truly experienced life?"
    ]
  },
  {
    title: "A Guide for the Perplexed: Conversations with Paul Cronin",
    author: "Werner Herzog",
    text: ["Our culture today, especially television, infantilises us. The indignity of it kills our imagination. May I propose a Herzog dictum? Those who read own the world. Those who watch television lose it. Sitting at home on your own, in front of the screen, is a very different experience from being in the communal spaces of the world, those centres of collective dreaming. Television creates loneliness. This is why sitcoms have added laughter tracks which try to cheat you out of your solitude. Television is a reflection of the world in which we live, designed to appeal to the lowest common denominator. It kills spontaneous imagination and destroys our ability to entertain ourselves, painfully erasing our patience and sensitivity to significant detail."]
  }
];

function getIndexFromInd(index) {
  var splittedArray = index.split("-");
  return Belt_Array.getExn(splittedArray, splittedArray.length - 1 | 0);
}

function appendId(id, index) {
  return id + "-" + index;
}

function postHeading(heading, className) {
  var headingTag = document.createElement("h2");
  headingTag.classList = className;
  headingTag.innerText = heading;
  return headingTag;
}

function postSubHeading(subHeading) {
  var subHeadingTag = document.createElement("h3");
  subHeadingTag.innerText = subHeading;
  return subHeadingTag;
}

function postParagraph(paraText, className) {
  var paraTag = document.createElement("p");
  paraTag.classList = className;
  paraTag.innerText = paraText;
  return paraTag;
}

function deletePara(title) {
  var paraTag = document.createElement("p");
  paraTag.classList = "text-center";
  var preText = document.createTextNode("This post from ");
  var empText = document.createElement("em");
  empText.innerText = title;
  var postText = document.createTextNode(" will be permanently removed in 10 seconds.");
  paraTag.appendChild(preText);
  paraTag.appendChild(empText);
  paraTag.appendChild(postText);
  return paraTag;
}

function restoreFn($$event, originalDiv, timeoutID) {
  console.log("Restore event called...");
  window.clearTimeout(timeoutID);
  var recoverElem = Caml_array.get($$event.path, 2);
  return recoverElem.replaceWith(originalDiv);
}

function deleteFn($$event, timeoutID) {
  console.log("Delete event called...");
  window.clearTimeout(timeoutID);
  var recoverElem = Caml_array.get($$event.path, 2);
  return recoverElem.remove();
}

function recoverDiv(index, title, originalDiv) {
  var deletedDiv = document.createElement("div");
  deletedDiv.classList = "post-deleted pt-1";
  deletedDiv.setAttribute("id", appendId("block", index));
  deletedDiv.appendChild(deletePara(title));
  var flexDiv = document.createElement("div");
  flexDiv.classList = "flex-center";
  var restoreBtn = document.createElement("button");
  restoreBtn.setAttribute("id", appendId("block-restore", index));
  restoreBtn.classList = "button button-warning mr-1";
  restoreBtn.innerText = "Restore";
  var timeoutID = window.setTimeout((function (param) {
          console.log("Timeout");
          return document.getElementById(deletedDiv.id).remove();
        }), 10000);
  restoreBtn.addEventListener("click", (function ($$event) {
          return restoreFn($$event, originalDiv, timeoutID);
        }));
  var delImmBtn = document.createElement("button");
  delImmBtn.setAttribute("id", appendId("block-delete-immediate", index));
  delImmBtn.classList = "button button-danger";
  delImmBtn.innerText = "Delete Immediately";
  delImmBtn.addEventListener("click", (function ($$event) {
          return deleteFn($$event, timeoutID);
        }));
  flexDiv.appendChild(restoreBtn);
  flexDiv.appendChild(delImmBtn);
  var progressDiv = document.createElement("div");
  progressDiv.classList = "post-deleted-progress";
  deletedDiv.appendChild(flexDiv);
  deletedDiv.appendChild(progressDiv);
  return deletedDiv;
}

function removeFn($$event) {
  console.log("Remove event called...");
  var divElem = Caml_array.get($$event.path, 1);
  var title = Caml_array.get(divElem.childNodes, 0).innerText;
  var index = getIndexFromInd(divElem.id);
  var recoverElem = recoverDiv(index, title, divElem);
  return divElem.replaceWith(recoverElem);
}

function postBtn(index) {
  var btn = document.createElement("button");
  btn.setAttribute("id", appendId("block-delete", index));
  btn.classList = "button button-danger";
  btn.innerText = "Remove";
  btn.addEventListener("click", removeFn);
  return btn;
}

function postDiv(index, post) {
  var mainDiv = document.createElement("div");
  mainDiv.classList = "post";
  mainDiv.setAttribute("id", appendId("block", index));
  mainDiv.appendChild(postHeading(post.title, "post-heading"));
  mainDiv.appendChild(postSubHeading(post.author));
  Belt_Array.forEach(post.text, (function (text) {
          return mainDiv.appendChild(postParagraph(text, "post-text"));
        }));
  mainDiv.appendChild(postBtn(index));
  return mainDiv;
}

Belt_Array.forEachWithIndex(posts, (function (index, post) {
        return document.body.appendChild(postDiv(String(index), post));
      }));

export {
  Post ,
  posts ,
  getIndexFromInd ,
  appendId ,
  postHeading ,
  postSubHeading ,
  postParagraph ,
  deletePara ,
  restoreFn ,
  deleteFn ,
  recoverDiv ,
  removeFn ,
  postBtn ,
  postDiv ,
  
}
/*  Not a pure module */
