'use strict';

var firebaseConfig = {
  apiKey: "AIzaSyAoAch27Z8AFCpJzXZAYJdOqXNcMsYzrQQ",
  authDomain: "customers-elm.firebaseapp.com",
  databaseURL: "https://customers-elm-default-rtdb.europe-west1.firebasedatabase.app",
  projectId: "customers-elm",
  storageBucket: "customers-elm.appspot.com",
  messagingSenderId: "483385945460",
  appId: "1:483385945460:web:418418501d9f156742d021"
};

var app = firebase.initializeApp(firebaseConfig);
var database = app.database();
var CUSTOMERREFPATH = "customers"

function addCustomer(customer){
  var promise = database
    .ref(CUSTOMERREFPATH)
    .push(customer);
  return promise;
}

function updateCustomer(customer){
  var id = customer.id;
  var promise = database
    .ref(CUSTOMERREFPATH + "/" + id)
    .set(customer);
  return promise;
}

function deleteCustomer(customer){
  var id = customer.id;
  var promise = database
    .ref(CUSTOMERREFPATH + "/" + id)
    .remove();
  return promise;
}

function customerListener(){
  return database.ref(CUSTOMERREFPATH);
}