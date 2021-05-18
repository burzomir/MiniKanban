import { Elm } from "./Main.elm";

const tutorialEntries =
  '[{"id":"4","title":"1. Add a new lane using the button in the top right corner"},{"id":"5","title":"2. Rename the new lane to Done (Click on the name of the lane)"},{"id":"6","title":"3. Drag and drop entries number 1, 2 and 3 from the Todo lane to the Done lane"},{"id":"7","title":"4. Change order of entries in the Done lane by dragging and dropping them onto other entries"},{"id":"8","title":"5. Reload the application - you changes should be restored after the reload"}]';
const entries = JSON.parse(
  localStorage.getItem("entriesCollection") || tutorialEntries
);

const tutorialLanes = '[{"id":"1","title":"Todo","entries":["4","5","6","7"]}]';
const lanes = JSON.parse(
  localStorage.getItem("lanesCollection") || tutorialLanes
);

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
