// This Example Comes from:
// https://deck.gl/showcases/gallery/viewport-transition

const {DeckGL, ScatterplotLayer, FlyToInterpolator} = deck;

const deckgl = new DeckGL({
      mapboxApiAccessToken: 'pk.eyJ1Ijoia3BpdmVydCIsImEiOiJjazc2dWc4YTUwMHp6M2tvNWIyYTQyaXNnIn0.MmXD8-ud_HmuDffvJMotVA',
      mapStyle: 'mapbox://styles/mapbox/light-v9',
      viewState: {
        longitude: r2d3.data[0].longitude,
        latitude: r2d3.data[0].latitude,
        zoom: 10
      },
      layers: [
        new ScatterplotLayer({
          data: r2d3.data,
          getPosition: d => [d.longitude, d.latitude],
          getColor: [255, 180, 0],
          radiusMinPixels: 10
        })
      ]
    });

    const inputs = r2d3.svg.selectAll('div')
      .data(r2d3.data)
      .append('g')
  /*    .enter().append('div'); 
      .enter().append(); */
      .enter();

  inputs.append('input')
      .attr('id', (d, i) => 'continent-' + i)
      .on('change', d => {
        deckgl.setProps({
          viewState: {
            longitude: r2d3.data[0].longitude,
            latitude: r2d3.data[0].latitude,
            zoom: 0,
            transitionInterpolator: new FlyToInterpolator(),
            transitionDuration: 5000
          }
        });
      });
  