import { Compartment, EditorState } from "@codemirror/state";
import { EditorView, lineNumbers } from "@codemirror/view";
import { basicSetup } from "codemirror";
import { boysAndGirls, tomorrow } from "thememirror";

const themeConfig = new Compartment();

function createEditorState(initialContents, myTheme) {
  let extensions = [basicSetup, themeConfig.of(themeIdentifier(myTheme))];

  return EditorState.create({
    doc: initialContents,
    extensions,
  });
}

function themeIdentifier(myTheme) {
  switch (myTheme) {
    case "light":
      return tomorrow;
    case "dark":
      return boysAndGirls;
    default:
      return tomorrow;
  }
}

function changeEditorTheme(myEditor, myTheme) {
  myEditor.dispatch({
    effects: themeConfig.reconfigure(themeIdentifier(myTheme)),
  });
}

function createEditorView(parent, initialDoc) {
  const initState = createEditorState(initialDoc, "light");
  const editorView = new EditorView({state: initState, parent: parent});
  window
    .matchMedia("(prefers-color-scheme: dark)")
    .addEventListener("change", (event) => {
      if (event.matches) {
        changeEditorTheme(editorView, "dark"); // Switch to dark mode
      } else {
        changeEditorTheme(editorView, "light"); // Switch to light mode
      }
    });

  if (window.matchMedia("(prefers-color-scheme: dark)").matches) {
    changeEditorTheme(editorView, "dark");
  } else {
    changeEditorTheme(editorView, "light");
  }
  return editorView;
}

export { createEditorView };
