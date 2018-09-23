'use strict';

require("./main.css")
const {Elm} = require("./Main.elm")

var app = Elm.Main.init({
  flags: {
    "seedInt": Date.now(),
    "storedBuilds" : JSON.parse(localStorage.getItem('storedBuilds')) || {},
  }
});

app.ports.textToSpeechQueue.subscribe(function(data) {
  data.forEach(function(phrase) {
    var speechSynthesizer = window.speechSynthesis;
    var utterance = new SpeechSynthesisUtterance(phrase);
    speechSynthesizer.speak(utterance);
  });
});

app.ports.buildsToStore.subscribe(function(data) {
  localStorage.setItem('storedBuilds', JSON.stringify(data));
});
