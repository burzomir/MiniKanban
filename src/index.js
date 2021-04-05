import { Elm } from "./Main.elm";

const collection = JSON.parse(
  localStorage.getItem("entriesCollection") || "[]"
);

const app = Elm.Main.init({
  node: document.querySelector("main"),
  flags: collection,
});

app.ports.saveEntriesCollection.subscribe((collection) =>
  localStorage.setItem("entriesCollection", JSON.stringify(collection))
);
