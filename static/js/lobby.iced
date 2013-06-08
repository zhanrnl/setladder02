# HELPERS
randomName = (n) ->
  c = -> String.fromCharCode(Math.floor(Math.random() * 26) + 97)
  (c() for i in [1..n]).join ''

class WSController
  constructor: (name) ->
    @host = 'ws://' + window.location.hostname + ':3001'
    @socket = new WebSocket(@host)
    @data = ko.observable '{}'
    
    @socket.onopen = =>
      console.log 'opening socket'
      @socket.send(name())
      # @socket.send('ping')
      
    @socket.onmessage = (message) =>
      console.log 'received message', message.data
      @data message.data

    @socket.onclose = =>
      console.log 'socket closed'

class AppVM
  constructor: ->
    @name = ko.observable randomName(10)
    @wscontroller = new WSController @name
    @nameList = ko.computed =>
      data = JSON.parse @wscontroller.data()
      if 'names' of data
        return data.names

$ -> 
  vm = new AppVM
  ko.applyBindings(vm)