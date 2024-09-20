// Adapted from:
//       https://github.com/webrtc/samples/blob/gh-pages/src/content/peerconnection/channel/js/main.js
//       https://github.com/webrtc/samples/blob/gh-pages/src/content/datachannel/basic/js/main.js

'use strict';

let localConnection;
let remoteConnection;
let sendChannel;
let receiveChannel;

function createConnection() {
    console.log('start connection');
    const servers = null;
    localConnection = new RTCPeerConnection(servers);

    // TODO remove global access before merge
    window.localConnection = localConnection;
    console.log('Created peer connection');

    // Pause place: can call the js from ocaml - now need to actually make work
    
    sendChannel = localConnection.createDataChannel('sendDataChannel');
    console.log('Created send data channel');

    localConnection.onicecandidate = e => {
       onIceCandidate(localConnection, e);
    };
    sendChannel.onopen = onSendChannelStateChange;
    sendChannel.onclose = onSendChannelStateChange;

    remoteConnection = new RTCPeerConnection(servers);
    // TODO remove global access before merge
    window.remoteConnection = remoteConnection;
    console.log('Created remote peer connection object remoteConnection');

    remoteConnection.onicecandidate = e => {
        onIceCandidate(remoteConnection, e);
    }
    remoteConnection.ondatachannel = receiveChannelCallback;
    
    localConnection.createOffer().then(
        gotDescription1,
        onCreateSessionDescriptionError
    )
}

function gotDescription1(desc) {
    localConnection.setLocalDescription(desc);
    console.log(`Offer from localConnection\n${desc.sdp}`);
    remoteConnection.setRemoteDescription(desc);
    remoteConnection.createAnswer().then(
        gotDescription2,
        onCreateSessionDescriptionError
    );
}

function gotDescription2(desc) {
    remoteConnection.setLocalDescription(desc);
    console.log(`Answer from remoteConnection\n${desc.sdp}`);
    localConnection.setRemoteDescription(desc);
}

function onIceCandidate(peerConnection, event) {
    getOtherPeerConnection(peerConnection)
        .addIceCandidate(event.candidate)
        .then(
            onAddIceCandidateSuccess,
            onAddIceCandidateError
        );
    console.log(`${getName(peerConnection)} ICE candidate: ${event.candidate ? event.candidate.candidate : '(null)'}`)
}

function getOtherPeerConnection(peerConnection) {
    return (peerConnection === localConnection) ? remoteConnection : localConnection;
}

function getName(peerConnection) {
    return (peerConnection === localConnection) ? 'localPeerConnection' : 'remotePeerConnection';
}

function onAddIceCandidateSuccess() {
    console.log('AddIceCandidate success.');
}

function onAddIceCandidateError(error) {
    console.log(`Failed to add Ice Candidate: ${error.toString()}`);
}

function receiveChannelCallback(event) {
    console.log('Receive Channel Callback');
    receiveChannel = event.channel;
    receiveChannel.onmessage = onReceiveMessageCallback;
    receiveChannel.onopen = onReceiveChannelStateChange;
    receiveChannel.onclose = onReceiveChannelStateChange;
}

function onReceiveMessageCallback(event) {
    console.log('Received Data: ' + event.data);
}

function onSendChannelStateChange() {
    const readyState = sendChannel.readyState;
    console.log('Send channel state is: ' + readyState);
    if (readyState === 'open') {
        
    } else {

    }
}

function onReceiveChannelStateChange() {
    const readyState = receiveChannel.readyState;
    console.log(`Receive channel state is: ${readyState}`);
}

function onCreateSessionDescriptionError(error) {
    console.log('Failed to create session description: ' + error.toString());
}

function sendActions(data) {
    sendChannel.send(data);
    console.log('Sent Data: ' + data);
}
