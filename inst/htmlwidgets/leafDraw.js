// arbitrary projected local tiles as leaflet map

HTMLWidgets.widget({

  name: 'leafDraw',

  type: 'output',

  initialize: function(el, width, height) {

    // we need a not htmlwidget div in the widget container
    addElement("lnlt");
    addElement("coords");

    // initialize the leaflet map staticly at the "el" object
    // hard-coding center/zoom here for a non-empty initial view, since there
    // is no way for htmlwidgets to pass initial params to initialize()
    // so we set maxbounds to the world and center somewhat at 0 Lat 0 Lon
    var southWest = L.latLng(-90, -180),
    northEast = L.latLng(90, 180),
    bounds = L.latLngBounds(southWest, northEast);
    var map = new L.map(el, {
      center: mapCenter,
      maxBounds: bounds,
      zoom: initialZoom
    });

    // we could add more (static) leaflet stuff here ;-)

    // The map is rendered staticly => so there would be no output binding
    // for the further handling we generate the binding to el this.getId(el)
    if (typeof this.getId === 'undefined') return map;
    map.id = this.getId(el);

    // Store the map on the element so we could find it later by ID
    $(el).data("leaflet-map", map);

    //return the initial mapsetup to the renderValue function
    return map;
  },

  renderValue: function(el, x, map) {
      return this.doRenderValue(el, x, map);
    },

  doRenderValue: function(el, x, map) {

      
    

   // we define the first layer of the list to be the default one
    var defaultLayer = L.tileLayer.provider(x.layer[0]).addTo(map);
    var baseLayers = {};
    for (var i = 0; i < x.layer.length;  i++) {
      baseLayers[x.layer[i] ] = L.tileLayer.provider(x.layer[i]);
      }
   // create draw layer
    var drawnItems = new L.FeatureGroup();
    map.addLayer(drawnItems);
  
  // layer control
   if (x.overlayLayer !== null){
    var layerControl = L.control.layers(baseLayers,overlayLayer).addTo(map);}
    else {
    var layerControl = L.control.layers(baseLayers).addTo(map);
  }
  
  // init draw control     
  var drawControl = new L.Control.Draw({
          position: x.position,
                 draw: {
                polyline: {
            shapeOptions: {
                color: '#FFFF00',
                weight: 2
            }},
                rectangle:x.rectangle,
                polygon: x.poly, 
                circle: x.circle,
                marker: x.point
            },
            edit: {
                featureGroup: drawnItems,
                remove: x.remove,
            }
        }).addTo(map);
    
    // Each time a feaute is created, it's added to the over arching feature group
     map.on('draw:created', function(e) {
            drawnItems.addLayer(e.layer);
        });
        

/*
    // create "save" link
        var b = document.createElement('a');
        var linkText = document.createTextNode("Save");
        b.appendChild(linkText);
        b.title = "save geoJson";
        b.href = "#";
        b.id = 'export';
        el.appendChild(b);
    
    // create "grab" link
        var a = document.createElement('a');
        var linkText = document.createTextNode("Grab");
        a.appendChild(linkText);
        a.title = "grab geoJson";
        a.href = "#";
        a.id = 'grabber';
        el.appendChild(a);
    // create "save" div container
        divInfo = document.createElement("div");
        divInfo.id ='export';
        el.appendChild(divInfo);
    // create "grab" div container
        divInfo = document.createElement("div");
        divInfo.id ='grabber';
        el.appendChild(divInfo);


        // Extract GeoJson from featureGroup
        var data = drawnItems.toGeoJSON();
        // Stringify the GeoJson
        var convertedData = JSON.stringify(data);
        var kml = tokml(data);
        var grabstring = "c(" + kml.replace(/ /g, ",") +")";
  */

    //  <span style="font: 15px " class="star" >&#x2704;</span>
    //'<span style="font-size: 20px" class="glyphicon glyphicon-download"></span>'
         L.easyButton(  '<span style="font-size: 5px font-weight:bold" class="glyphicon glyphicon-download"> JS</span>',
         function(){
           var data = drawnItems.toGeoJSON();
           // Stringify the GeoJson
           var convertedData = JSON.stringify(data);
           // Create ajax export using download.js
           download(new Blob([convertedData]), "dlTextBlob.txt", "text/plain");}).addTo(map);
     
     

     // if clickevent on "grabber"
     // document.getElementById('grabber').onclick = function(e) {
        L.easyButton(  '<span style="font-size: 10px " class="glyphicon glyphicon-download"> KML</span>',
         function(){
              var data = drawnItems.toGeoJSON();
        // Stringify the GeoJson
        var convertedData = JSON.stringify(data);
        var kml = tokml(data);
        //var grabstring = "c(" + kml.replace(/ /g, ",") +")";
        //document.write(convertedData);
        //document.getElementById("coords").innerHTML = '<div class="coords"' + grabstring + '"</div>"' 
        //window.alert(grabstring);}).addTo(map);
       download(new Blob([kml]), "dlTextBlob.txt", "text/plain");}).addTo(map);
      //}

  // grab the lnlt div and put the mousmove output there
  lnlt = document.getElementById('lnlt');
  map.on('mousemove', function (e) {
        lnlt.textContent =
                " Latitude: " + (e.latlng.lat).toFixed(5)
                + " | Longitude: " + (e.latlng.lng).toFixed(5)
                + " | Zoom: " + map.getZoom() + " ";
  });

},


resize: function(el, width, height, instance) {
}
});

  // get the files and returns them as text stream
  function wget(urls, fn) {
        var results = [],
            lookup = {},
            complete = 0,
            total = urls.length;

        urls.forEach(function(url) {
            var i = lookup[url] = results.length,
                request = new XMLHttpRequest();
            results.push(null);
            request.open('GET', url, true);
            request.onload = function () {
                if (request.status < 200 && request.status > 400) return;
                results[i] = request.responseText;
                complete++;
                if (complete === total) fn.apply(null, results);
            };
            request.send();
        });
    }


// we need a new div element because we have to handle
// the mouseover output seperatly
function addElement(id) {
  // generate new div Element
  var newDiv = document.createElement("div");
  // insert to DOM
  document.body.insertBefore(newDiv, null);
      //provide ID and style
      newDiv.id = id;
      //newDiv.style.cssText = css;
}