// -*- coding: latin-1 -*-

// This file is part of RefactorErl.
//
// RefactorErl is free software: you can redistribute it and/or modify
// it under the terms of the GNU Lesser General Public License as published
// by the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// RefactorErl is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public License
// along with RefactorErl.  If not, see <http://plc.inf.elte.hu/erlang/>.
//
// The Original Code is RefactorErl.
//
// The Initial Developer of the Original Code is Eötvös Loránd University.
// Portions created  by Eötvös Loránd University and ELTE-Soft Ltd.
// are Copyright 2007-2013 Eötvös Loránd University, ELTE-Soft Ltd.
// and Ericsson Hungary. All Rights Reserved.

//Author: Mátyás Kuti

#include "dependencygraph.h"

DependencyGraph::DependencyGraph(const double &w, const double &h) :
    width_(w), height_(h)
{
}

const QMap<QString, Node*> &DependencyGraph::GetNodes() const
{
    return nodes_;
}

const QMap<QString, QList<Edge *> > &DependencyGraph::GetEdges() const
{
    return adj_;
}

void DependencyGraph::AddNode(Node *node)
{
    nodes_[node->GetId()] = node;
}

bool DependencyGraph::AddEdge(const QString &from_id, const QString &to_id)
{
    if( nodes_.count(from_id) > 0 && nodes_.count(to_id) > 0 ) {
        adj_[from_id].push_back( new Edge( nodes_[from_id], nodes_[to_id] ) );
        return true;
    }
    return false;
}


void DependencyGraph::SetSize(const double &w, const double &h)
{
    width_ = w;
    height_ = h;
}

const double &DependencyGraph::Width() const
{
    return width_;
}

const double &DependencyGraph::Height() const
{
    return height_;
}
