import { Elm } from "./Main.elm";

const entries = JSON.parse(localStorage.getItem("entriesCollection") || "[]");

const lanes = JSON.parse(localStorage.getItem("lanesCollection") || "[]");

const app = Elm.Main.init({
  node: document.querySelector("main"),
  flags: { entries, lanes },
});

app.ports.saveEntriesCollection.subscribe((collection) =>
  localStorage.setItem("entriesCollection", JSON.stringify(collection))
);

app.ports.saveLanesCollection.subscribe((collection) =>
  localStorage.setItem("lanesCollection", JSON.stringify(collection))
);
