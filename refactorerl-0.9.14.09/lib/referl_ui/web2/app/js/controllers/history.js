'use strict';

function HistoryCtrl($scope, $http, smartLocation, $location, jobs, $document, $timeout) {

	$scope.history = [];
	$scope.historyType = $scope.historyType || "";
	$scope.data = {loading: true};
    $scope.ReFollowing = {window: null, ids: [], pos: {top:0, left:0}};
    $scope.shownSelection = "";

    $scope.itemProp = function(itemId, prop) {
    	if (typeof itemId == 'undefined') {
    		return undefined;
    	}
    	var item = _.find($scope.data.items, function(item){ return item.id == itemId; });
    	return (item) ? item[prop] : undefined;
    };

    $scope.itemParams = function(itemId) {
    	return $scope.itemProp(itemId, "parameters");
    };

    $scope.showSelection = function(itemId){
    	var params = $scope.itemParams(itemId);
    	if(!params) {
    		return true;
    	}

    	if(!$scope.itemProp(itemId, "dbchanged")) {
    		return true;
    	}

		$scope.shownSelection = params.text;
    }

    $scope.resetShownSelection = function() {
    	$timeout(function() {
    		$scope.shownSelection = "";
    	}, 100);
    }

    $scope.historyHref = function(itemId, linkTo){
    	var params = $scope.itemParams(itemId);
    	if(!params) {
    		return "";
    	}

    	if($scope.itemProp(itemId, "dbchanged")) {
    		return "";
    	}

    	if(linkTo == "file") {
    		return "#/database?file=" + params.file;
    	}

    	return "#/database?file=" + params.file + "&pos=" + params['fromIndex'] + "," + params['toIndex'];
    }

	var formatDate = function(values) {
		var secs = values[0] * 1000000 + values[1];
		var date = new Date(secs * 1000);
		return moment(date).calendar();
	};

	var load = function(promise) {
		promise.then(
			function(response) {
				angular.forEach(response.data.items, function(item) {
					item.launchedDate = formatDate(item.launched);
					if (item.finished) {
						item.finishedDate = formatDate(item.finished);
					}
				});
				var data = response.data;
				data.loading = false;
				$scope.data = data;
			},
			function(error) {
				var data = {};
				data.failed = error;
				data.loading = false;
				$scope.data = data;
			}
		);
	};

	var reload = function() {
		load($http.post("api/history", {
			type: $scope.historyType
		}));
	};

	$scope.$watch(smartLocation.getJobId, function(jobId) {
		$scope.currentId = jobId;
	});

	$scope.$on("jobs:started", reload);
	$scope.$on("jobs:finished", reload);
	$scope.$on("jobs:backgroundChange", reload);

	$scope.href = function(item) {
		return "#/" + item.page + "?" + smartLocation.createJobIdPart(item.id);
	};

	$scope.click = function(item) {
		if (item.removed) return;
		smartLocation.setPage(item.page);
		smartLocation.setJobId(item.id);
	};

	$scope.remove = function(job) {
		job.removed = true;
		jobs.remove(job.id).then(reload);
	};

	reload();

}
